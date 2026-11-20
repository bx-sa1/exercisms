module Anagram (anagramsFor) where

import Data.Char
import Data.List

compareStr :: String -> String -> Bool
compareStr a b
  | a == b = False
  | length a /= length b = False
  | otherwise = sort a == sort b

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = [ y | y <- xss, compareStr (map toLower xs) (map toLower y)]
