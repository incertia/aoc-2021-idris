module AOC2021.Day02

import AOC2021

import Data.Either
import Data.List
import Data.Maybe
import Data.String
import Data.String.Parser

data Command = Forward Integer | Up Integer | Down Integer

Show Command where
  show (Forward n) = "Forward " ++ show n
  show (Up n) = "Up " ++ show n
  show (Down n) = "Down " ++ show n

parseCommandPrim : String -> (Integer -> Command) -> Parser Command
parseCommandPrim cmd f = map f $ spaces *> token cmd *> integer <* spaces

parseForward : Parser Command
parseForward = parseCommandPrim "forward" Forward

parseUp : Parser Command
parseUp = parseCommandPrim "up" Up

parseDown : Parser Command
parseDown = parseCommandPrim "down" Down

parseCommand : Parser Command
parseCommand = parseForward <|> parseUp <|> parseDown

export
solve02 : Part -> String -> String
solve02 part input = show . (\(d, p, _) => d * p) . foldl f (0, 0, 0) $ commands
  where
    commands : List Command
    commands = lowerMaybe . getRight . mapSnd fst . parse (many parseCommand) $ input
    f : (Integer, Integer, Integer) -> Command -> (Integer, Integer, Integer)
    f (depth, pos, aim) cmd =
      case (cmd, part) of
        (Forward n, PartA) => (depth, pos + n, aim)
        (Down n, PartA) => (depth + n, pos, aim)
        (Up n, PartA) => (depth - n, pos, aim)
        (Forward n, PartB) => (depth + aim * n, pos + n, aim)
        (Down n, PartB) => (depth, pos, aim + n)
        (Up n, PartB) => (depth, pos, aim - n)
