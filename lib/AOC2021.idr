module AOC2021

import Data.String.Parser

export
data Part = PartA | PartB

parsePartA : Parser Part
parsePartA = string "PartA" *> pure PartA

parsePartB : Parser Part
parsePartB = string "PartB" *> pure PartB

export
parsePart : String -> Either String Part
parsePart = mapSnd fst . parse ((parsePartA <|> parsePartB) <?> "expected \"PartA\" or \"PartB\"")

export
Show Part where
  show PartA = "PartA"
  show PartB = "PartB"
