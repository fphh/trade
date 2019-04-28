

module Trade.Strategy.Condition where

import Control.Applicative (liftA2)

import Control.Monad.State (State)

import qualified Data.List as List

import Trade.Type.DisInvest (DisInvest(..))

import Trade.Strategy.Type (IndexedSignals)



data Implication =
  Maybe Bool :-> DisInvest
  deriving (Show)


(.||) :: Maybe Bool -> Maybe Bool -> Maybe Bool
(.||) = liftA2 (||)

infixr 2 .||

(.&&) :: Maybe Bool -> Maybe Bool -> Maybe Bool
(.&&) = liftA2 (&&)

infixr 3 .&&

(.<) :: (Ord a) => Maybe a -> Maybe a -> Maybe Bool
(.<) = liftA2 (<)

(.>) :: (Ord a) => Maybe a -> Maybe a -> Maybe Bool
(.>) = liftA2 (>)

(.=) :: (Ord a) => Maybe a -> Maybe a -> Maybe Bool
(.=) = liftA2 (==)

infix 4 .<, .>, .=


(.+) :: (Num a) => Maybe a -> Maybe a -> Maybe a
(.+) = liftA2 (+)

(.-) :: (Num a) => Maybe a -> Maybe a -> Maybe a
(.-) = liftA2 (-)

infixl 6 .+



symbol :: (a, b) -> a
symbol = fst

constant :: a -> Maybe a
constant = Just


data Condition sym =
  sym :=: [Implication]
  | Condition sym :|: Condition sym
  deriving (Show)


infixr 0 :|:
infix 1 :=:
infix 1 :->

conditions :: Condition sym -> State (IndexedSignals sym t x) [(sym, DisInvest)]
conditions (sym :=: imps) =
  let f (_ :-> bs) = [(sym, bs)]
      g (Just b :-> _) = b
      g _ = False
  in return (maybe [] f (List.find g imps))

conditions (t :|: s) = liftA2 (++) (conditions t) (conditions s)
