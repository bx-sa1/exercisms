module Darts (score) where

outerRadius :: Float
outerRadius = 10

middleRadius :: Float
middleRadius = 5

innerRadius :: Float
innerRadius = 1

circlePointTest :: Float -> Float -> Float -> Bool
circlePointTest r x y = x * x + y * y <= r * r

score :: Float -> Float -> Int
score x y = if circlePointTest innerRadius x y
  then 10
  else if circlePointTest middleRadius x y
  then 5
  else if circlePointTest outerRadius x y
  then 1
  else 0
