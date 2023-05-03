module ENSICO where

-- import Cp
import Data.Char
import Data.List
--import Data.List.Split

--- composition ---

(|>) = flip ($)

--- Functional Programming, part II

help x = splitAt (div (length x) 2) x

divide = help

split = help

part1 = fst . divide
part2 = snd . divide

--- Dictionaries

a # b = a `zip` b
a |-> b = (a,b)
set = sort . nub
collect :: (Ord b, Ord a) => [(b, a)] -> [(b, [a])]
collect x = set [ k |-> set [ d' | (k',d') <- x , k'==k ] | (k,d) <- x ]

