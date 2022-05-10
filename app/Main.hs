module Main (main) where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad (guard, replicateM, (>=>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT)
import Data.List (elemIndex)
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    noCompletion,
    outputStrLn,
    runInputT,
    setComplete,
  )
import System.Random.Stateful
  ( StateGenM,
    StdGen,
    UniformRange (uniformRM),
    initStdGen,
    runStateGenT_,
  )
import Text.Read (readMaybe)

data Expr = Roll Word Word | Constant Word

main :: IO ()
main =
  initStdGen
    >>= flip
      runStateGenT_
      (runInputT (setComplete noCompletion defaultSettings) . program)

program :: StateGenM StdGen -> InputT (StateT StdGen IO) ()
program gen =
  getInputLine "> "
    >>= maybe
      (return ())
      (lift . runLine gen >=> outputStrLn >=> const (program gen))

runLine :: StateGenM StdGen -> String -> StateT StdGen IO String
runLine gen =
  maybe (return "Invalid Input") (fmap show . calcExprs gen) . parseExprs

calcExprs :: StateGenM StdGen -> [Expr] -> StateT StdGen IO Word
calcExprs gen = fmap sum . mapM (calcExpr gen)

calcExpr :: StateGenM StdGen -> Expr -> StateT StdGen IO Word
calcExpr _ (Constant x) = return x
calcExpr gen (Roll numDice numSides) =
  fmap sum $ replicateM (fromIntegral numDice) $ uniformRM (1, numSides) gen

parseExprs :: String -> Maybe [Expr]
parseExprs = mapM parseExpr . words

parseExpr :: String -> Maybe Expr
parseExpr input = parseRoll input <|> parseConstant input

parseRoll :: String -> Maybe Expr
parseRoll input = do
  index <- elemIndex 'd' input
  numDice <- readMaybe $ take index input
  numSides <- readMaybe $ drop (index + 1) input
  guard $ numSides > 0
  return $ Roll numDice numSides

parseConstant :: String -> Maybe Expr
parseConstant = fmap Constant . readMaybe
