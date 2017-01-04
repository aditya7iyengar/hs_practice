module Codewars.Exercise.Lucas where

lucasnum :: Int -> Integer
lucasnum n = if n >= 0 then cache !! n else (-1) ^ (-n) * (cache !! (-n))
cache = (2:1:zipWith (+) cache (tail cache))
