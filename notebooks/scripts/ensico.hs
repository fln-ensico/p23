module ENSICO where

--import Cp
--import Data.Char
--import Data.List
--import Data.List.Split

--- composition ---

(|>) = flip ($)

--- Functional Programming, part II

help x = splitAt (div (length x) 2) x

divide = help

split = help

part1 = fst . divide
part2 = snd . divide

