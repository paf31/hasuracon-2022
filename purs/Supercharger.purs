module Supercharger 
  ( Config, Column, Predicate, Order
  , _eq, _neq, _in, _is_null, _is_not_null, _lt, _lte, _gt, _gte, _and, _or, _not
  , binaryAnd, binaryOr
  , (==), (!=), (<), (<=), (>), (>=), (&&), (||)
  , asc, desc
  , module Reexports
  ) where

import Prelude (($), show) as Reexports
import Data.Maybe (Maybe(..)) as Reexports

foreign import data Column :: Type -> Type

foreign import data Predicate :: Type
foreign import data Order :: Type

foreign import _eq :: forall a. Column a -> a -> Predicate
foreign import _neq :: forall a. Column a -> a -> Predicate
foreign import _in :: forall a. Column a -> Array a -> Predicate
foreign import _is_null :: forall a. Column a -> Predicate
foreign import _is_not_null :: forall a. Column a -> Predicate
foreign import _lt :: forall a. Column a -> a -> Predicate
foreign import _lte :: forall a. Column a -> a -> Predicate
foreign import _gt :: forall a. Column a -> a -> Predicate
foreign import _gte :: forall a. Column a -> a -> Predicate
foreign import _and :: Array Predicate -> Predicate
foreign import _or :: Array Predicate -> Predicate
foreign import _not :: Predicate -> Predicate

binaryAnd :: Predicate -> Predicate -> Predicate
binaryAnd x y = _and [x, y]

binaryOr :: Predicate -> Predicate -> Predicate
binaryOr x y = _or [x, y]

infix 4 _eq as ==
infix 4 _neq as !=
infixl 4 _lt as <
infixl 4 _lte as <=
infixl 4 _gt as >
infixl 4 _gte as >=
infixr 3 binaryAnd as &&
infixr 2 binaryOr as ||

foreign import asc :: forall a. Column a -> Order
foreign import desc :: forall a. Column a -> Order