module Lec7 (
  Shape(..),
  Point(..),
  Person(..),
  Day(..),
  area,
  mapToRadius,
  baseRect,
  nudge,
  tellPerson,
  lockerLookup
) where

import Text.Printf (printf)
import qualified Data.Map as Map

-- 새로운 데이터 타입을 정의할 때는 data keyword를 사용한다.
-- data Shape = Circle Float Float Float | Rect Float Float Float Float
--   deriving (Show)
-- Type 동의어 (type synonym)을 정의할 때는 type keyword를 사용한다.
-- type String = [Char]

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
-- Rect (Point 20 30) (Point 30 50)
-- nudge (Circle (Point 10 10 ) 20) 20 30
-- Circle (Point 30 40) 20

-- Record syntax
-- 아래처럼 길게 늘여 쓸 수 있지만 사람에 대한 특정 정보를 얻는 함수 등을 만들어야 할때 까다롭다.
-- data Person = Person String String Int Float String String deriving (Show)
-- guy = Person "test" "test2" 43 184 "555-5555" "Cake"
-- firstName :: Person -> String
-- firstName (Person firstname _ _ _ _ _) = firstname
-- lastName :: Person -> String
-- lastName (Person _ lastname _ _ _ _) = lastname

-- 레코드 구문을 써보자!
-- indent에 주의하자!
-- 하스켈은 각 필드에 대한 getter를 자동으로 만든다.
data Person = Person { firstName :: String
                      ,lastName :: String
                      ,age :: Int
                      ,height :: Float
                      ,phoneNumber :: String
                      ,flavor :: String
                    } deriving (Show)

-- guy = Person { firstName="test"
--               ,lastName="test2"
--               ,age=25
--               ,height=184.5
--               ,phoneNumber="555-5555"
--             }
-- :t firstName :: Person -> String
-- firstName guy
-- "test"

tellPerson :: Person -> String
tellPerson Person {firstName = f, lastName = l, age = a} =
  printf "%s %s has %d years old." f l a

-- Day type은 Eq, Bounded, Enum등의 타입클래스에 속하기 때문에 여러가지 연산이 가능하다.
-- "상속"의 느낌으로 알고있으면 좋을 것 같다.
data Day = Mon | Tue | Wed | Thur | Fri | Sat | Sun
          deriving (Eq, Show, Read, Bounded, Enum)
-- (minBound :: Day) `shouldBe` Mon > Bounded 타입 클래스의 기능
-- succ Mon `shouldBe` Tue > Enum, Eq 타입 클래스의 기능
-- ([minBound .. maxBound] :: [Day]) `shouldBe` [Mon, Tue, Wed, Thur, Fri, Sat, Sun] > Enum 타입 클래스의 기능

-- Either a b = Left a | Right b
-- Nothing 같은 Maybe type으로 실패에 대한 이유를 기술해야할 때 사용한다.
-- 실패는 Left, 성공은 Right type에 기술해야한다.

data LockerState = Taken | Free deriving (Show, Eq)
type LockerMap = Map.Map Int (LockerState, String)
type Err = String

lockerLookup :: Int -> LockerMap -> Either Err String
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
  Nothing -> Left "not exist"
  Just (state, code) -> if state /= Taken
                          then Right code
                          else Left "already taken"

