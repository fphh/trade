

module Trade.Strategy.Condition where

import Control.Applicative (liftA2)

import Control.Monad.State (State)

import qualified Data.List as List

import Trade.Type.Position (Position(..))

import Trade.Strategy.Type (IndexedSignals)



data Implication =
  Maybe Bool :-> Position
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

cst :: Double -> Maybe Double
cst = Just


data Condition sym =
  sym :=: [Implication]
  | Condition sym :|: Condition sym
  deriving (Show)


infixr 0 :|:
infix 1 :=:
infix 1 :->


eval :: Condition sym -> State (IndexedSignals sym t x) [(sym, Position)]
eval (sym :=: imps) =
  let f (_ :-> bs) = [(sym, bs)]
      g (Just b :-> _) = b
      g _ = False
  in return (maybe [] f (List.find g imps))

eval (t :|: s) = liftA2 (++) (eval t) (eval s)
