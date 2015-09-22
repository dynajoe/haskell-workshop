module Types where

-- Sum types (enumerations are a special case)
data Color = Red | Green | Blue

data List a = Nil | Cons a (List a) deriving (Show)

-- Type Alias
type FirstName = String

-- Product Type (record)
data Meetup = MU {
    name :: String,
    location :: String
} deriving(Show)

newtype Person = Person {
    firstName :: FirstName
} deriving (Show)

-- Type classes
class FooBar a where
    foo :: a -> String

-- Type class implementations
instance FooBar Float where
    foo _ = "foobar"

addOne :: (Num a) => List a -> List a
addOne (Cons x xs) = Cons (x + 1) (addOne xs)
addOne Nil = Nil
