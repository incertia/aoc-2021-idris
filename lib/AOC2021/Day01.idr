module AOC2021.Day01

import AOC2021

import Data.List
import Data.String
import Data.Zippable

export
solve01 : Part -> String -> String
solve01 part input = show . length . filter id $ zipWith (<) depths (drop 1 depths)
  where
    l : List Integer
    l = cast <$> lines input
    windowed : List Integer
    windowed = zipWith3 (\a => \b => \c => a + b + c) l (drop 1 l) (drop 2 l)
    depths : List Integer
    depths = case part of
                  PartA => l
                  PartB => windowed
