module Core where
import Prelude (Show)
import qualified Prelude
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
natToInt :: Nat -> Prelude.Int
natToInt = \n -> case n of
  Zero -> 0
  Succ a -> 1 Prelude.+ natToInt a
instance Prelude.Show Nat where
  show = \n -> Prelude.show (natToInt n)
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
nats :: Stream
nats =
  natsFrom Zero
natsFrom :: Nat -> Stream
natsFrom = \n ->
  Neck n (natsFrom (Succ n))
mapStream :: (Nat -> Nat) -> (Stream -> Stream)
mapStream =
  \f s ->
    Neck (f (head s)) (mapStream f (tail s))
filterStream :: (Nat -> Bool) -> (Stream -> Stream)
filterStream = 
  \p s ->
    case s of
      (Neck x xs) ->
        case p x of
          True -> (Neck x (filterStream p xs)) 
          False -> filterStream p xs
data List =
  Nil | Cons Nat List
  deriving Show
  
one = Succ Zero
two = Succ one
three = Succ two
four = Succ three
five =   Succ four
six = Succ five
seven = Succ six 
  
favnums =
 Cons four (Cons five (Cons seven (Nil)))
mapList :: (Nat -> Nat) -> (List -> List)
mapList =
  \n l ->
     case l of
       (Cons x xs) -> Cons (n x) (mapList n xs)
       Nil -> Nil
filterList ::(Nat -> Bool) -> (List -> List)
filterList =
  \p l ->
    case l of
      (Cons x xs) ->
         case p x of
           True -> (Cons x (filterList p xs))
           False -> filterList p xs
      Nil -> Nil
      
           
    





