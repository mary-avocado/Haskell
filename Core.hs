module Core where
import Prelude (Show)
data Bool = 
  False | True
  deriving Show
not :: Bool -> Bool
not =
  \x -> 
    case x of
      False -> True
      True -> False
id :: Bool -> Bool
id = \x -> x
notnot :: Bool -> Bool
notnot = 
  \x -> not (not x)
(&&) :: Bool -> (Bool -> Bool)
(&&) = 
  \x -> \y ->
    case x of
      False -> False
      True ->
        case y of
          False -> False
          True -> True
(||) :: Bool -> (Bool -> Bool)
(||) = \x y ->
  case x of
    True -> True
    False ->
      case y of
        True -> True
        False -> False
data Nat = Zero | Succ Nat
  deriving Show
(+) :: Nat -> Nat -> Nat
(+) = 
  \x y ->
    case x of
      Zero -> y
      (Succ a) -> Succ (a + y) 
even :: Nat -> Bool
even =
  \x ->
    case x of
      Zero -> True
      (Succ a) -> odd a
odd :: Nat -> Bool
odd =
  \x ->
    case x of
      Zero -> False
      (Succ a) -> even a
data Stream = Neck Nat Stream 
  deriving Show
zeroes :: Stream
zeroes = Neck Zero zeroes
head :: Stream -> Nat
head = \s ->
  case s of
    (Neck x xs) -> x
tail :: Stream -> Stream
tail =
  \s ->
    case s of
      (Neck x xs) -> xs  
      

