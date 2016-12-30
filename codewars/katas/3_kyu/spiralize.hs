module Spiral where

spiralize :: Int -> [[Int]]
spiralize 0 = []
spiralize 1 = [[1]]
spiralize n = [replicate n 1, replicate (n - 1) 0 ++ [1]] ++ sub
  where reversed = reverse . map reverse
        temp = zipWith (:) (repeat 1) . zipWith (:) (1 : repeat 0)
        sub = reversed . temp . spiralize $ n - 2
