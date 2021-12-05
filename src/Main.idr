module Main

import Data.Either
import Data.List
import Data.Maybe
import Data.SortedMap
import Data.String
import Data.String.Extra
import System
import System.File.ReadWrite

import AOC2021
import AOC2021.Day01
import AOC2021.Day02

solvers : SortedMap Integer (Part -> String -> String)
solvers = fromList
  [ (01, solve01), (02, solve02)
  ]

pad : Nat -> String -> String
pad n s = replicate (minus n (length s)) '0' ++ s

main : IO ()
main = do
  args <- getArgs
  case args of
    [_, prob, part] => do
      case (parsePositive prob, parsePart part) of
        (Just prob, Right part) =>
          case lookup prob solvers of
            Just solve => do
              input <- lowerMaybe . getRight <$> readFile ("input/" ++ pad 2 (show prob))
              putStrLn $ solve part input
            _ => putStrLn "bad"
        _ => putStrLn "bad"
    _ => putStrLn "bad"
  pure ()
