{-# LANGUAGE DeriveFunctor,       TypeFamilies        #-}
{-# LANGUAGE ScopedTypeVariables, RankNTypes          #-}
{-# LANGUAGE TypeApplications,    AllowAmbiguousTypes #-}

module Memo2
where

import Numeric.Natural
import Data.Kind (Type)


data Stream a = a :> Stream a deriving Functor

infixr 5 :>


class Functor f => Representable f where
 type Key f :: Type
 tabulate   :: (Key f -> a) -> f a
 index      :: f a          -> (Key f -> a)
-- Class laws:
-- index    . tabulate = id
-- tabulate . index    = id


instance Representable Stream where
 type Key Stream = Natural
 index :: Stream a -> Key Stream -> a
 index    = streamIndex
 tabulate :: (Key Stream -> a) -> Stream a
 tabulate = streamTabulate


instance Representable ((->) k) where
 type Key ((->) k) = k
 index :: (k -> a) -> Key ((->) k) -> a
 index    = id
 tabulate :: (Key ((->) k) -> a) -> k -> a
 tabulate = id 


data Tree a = Node a (Tree a) (Tree a) deriving Functor


data Eveness = Even | Odd


instance Representable Tree where
  type Key Tree = Natural
  tabulate :: (Key Tree -> a) -> Tree a
  tabulate f = fmap f nats where
    nats = Node 0 (fmap (\ n -> 2*n + 1) nats) (fmap (\ n -> 2*n + 2) nats)

  index :: Tree a -> Key Tree -> a
  index (Node a _ _) 0 = a
  index (Node _ l r) n = case evenness n of
    Odd  -> index l (div n 2)
    Even -> index r (div n 2 - 1)


-- Access the nth value stored in a Stream
streamIndex :: Stream a -> (Natural -> a)
streamIndex (x :> _)  0 = x
streamIndex (_ :> xs) n = streamIndex xs (n-1)

-- Take f to [f 0, f 1, f 2, f 3, ...]
streamTabulate :: (Natural -> a) -> Stream a
streamTabulate f = fmap f naturals where
  naturals = 0 :> fmap (+1) naturals


fibRec :: Num a => Natural -> a
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec (n-1) + fibRec (n-2)

fibOp :: Num a => (Natural -> a) -> (Natural -> a)
fibOp v 0 = 0
fibOp v 1 = 1
fibOp v n = v (n-1) + v (n-2)

fix f = let x = f x in x

fibNaive :: Num a => Natural -> a
fibNaive = fix fibOp  -- same as fibRec


streamMemoize :: ((Natural -> a) -> Natural -> a) -> Natural -> a
streamMemoize f = fix (streamIndex . streamTabulate . f)

fibSmart :: Num a => Natural -> a
fibSmart = streamMemoize fibOp 


-- | Memoize a recursive procedure using a Representable container of your choice.
memoize :: forall f a. Representable f => ((Key f -> a) -> (Key f -> a)) -> (Key f -> a)
memoize g = fix (index @f . tabulate . g)

fibSmart' :: Num a => Natural -> a
fibSmart' = memoize @Stream fibOp

fibNaive' :: Num a => Natural -> a
fibNaive' = memoize @((->) Natural) fibOp


evenness :: Integral a => a -> Eveness
evenness n = if odd n then Odd else Even


fibTree :: Num a => Natural -> a
fibTree = memoize @Tree fibOp