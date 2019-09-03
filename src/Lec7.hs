module Lec7 (
  Shape(..),
  Point(..),
  area,
  mapToRadius
) where

-- 새로운 데이터 타입을 정의할 때는 data keyword를 사용한다.
-- data Shape = Circle Float Float Float | Rect Float Float Float Float
--   deriving (Show)
-- Circle, Rect와 같은 '값 생성자'는 실제로 데이터 타입을 반환하는 '함수'다.
-- :t Circle :: Float -> Float -> Float -> Shape

-- Point type으로 개선하기
data Point = Point Float Float deriving (Show, Eq)
data Shape = Circle Point Float | Rect Point Point deriving (Show, Eq)

-- area는 Shape type을 받아 Float을 return하는 함수다.
-- '값 생성자'는 패턴매칭으로 동작할 수 있다.
area :: Shape -> Float
-- area (Circle _ _ r) = pi * r ^ 2
-- area (Rect x1 y1 x2 y2) = abs (x2 - x1) * abs (y2 - y1)

area (Circle _ r) = pi * r ^ 2
area (Rect (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

-- '값 생성자'는 함수이기 때문에 다음과 같은 mapper함수를 작성할 수도 있다.
-- 즉, currying이 가능하다.
mapToRadius :: [Float] -> [Shape]
mapToRadius = map $ Circle (Point 10 20)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rect (Point x1 y1) (Point x2 y2)) a b = Rect (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

-- 0, 0에 위치한 도형 그리기
baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect width height = Rect (Point 0 0) (Point width height)
-- Rect 0 0 width height

-- 0, 0에 위치했던 도형들을 옮긴다
-- nudge (baseRect 10 20) 20 30
-- Rect 20 30 30 50
