module Geometry.Sphere (
  volume,
  area
) where

volume :: Float -> Float
volume rad = (4.0 / 3.0) * pi * (rad ^ 3)

area :: Float -> Float
area rad = 4 * pi * (rad ^ 2)