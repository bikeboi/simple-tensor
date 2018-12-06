{-# LANGUAGE DataKinds, KindSignatures #-}

module Data.Simple.Vector where

import GHC.TypeLits
import Data.Proxy
import qualified Data.Vector as V

-- |Vector datatype. It's basically newtype wrapper with a type-level Nat
-- around a boxed vector from the 'Data.Vector' package.
newtype Vector (n :: Nat) a =
 Vector (V.Vector a) deriving Show

-- | Int vector
type IVec n = Vector n Int
-- | Float vector
type FVec n = Vector n Float
-- | Double vector
type DVec n = Vector n Double

unsafeTagVector :: KnownNat n => Proxy n -> [a] -> Vector n a
unsafeTagVector _ xs =Vector $ V.fromList xs

-- Smart constructors
-- | Make vector given a known length and a generation function
mkVector :: KnownNat n => Proxy n -> (Integer -> a) -> Vector n a  
mkVector p f = Vector $ V.fromList $ map f [1 .. natVal p] 

-- | Make a vector given a known length, a default value and a list.
-- If the list is too long, it gets truncated. If it's too short,
-- the remaining length is filled with the default value.
fromList :: KnownNat n => a -> Proxy n -> ([a] -> Vector n a)
fromList d p = \xs -> let xs' = take expLen xs
                          expLen = fromInteger $ natVal p
                      in unsafeTagVector p (fill expLen xs')
  where fill x as = let len = length as
                    in if len < x
                       then as ++ replicate (x - len) d else as

-- | Specialized version of 'fromList' for 'Num' types. The default
-- value is always 0
(|>) :: (Num a,KnownNat n) => Proxy n -> [a] -> Vector n a
(|>) = fromList 0

-- **Vector operations
-- | Addition
(<+>) :: Num a => Vector m a -> Vector m a -> Vector m a
(<+>) (Vector xs) (Vector ys) = Vector $ V.zipWith (+) xs ys

-- | Subtraction
(<->) :: Num a => Vector n a -> Vector n a -> Vector n a
(<->) (Vector v) (Vector v') = Vector $ V.zipWith (-) v v'

-- | Dot product
(<.>) :: Num a => Vector n a -> Vector n a -> a
(<.>) (Vector v) (Vector v') = V.sum $ V.zipWith (*) v v' 

-- | Scalar multiplication
(<^>) :: Num a => a -> Vector n a -> Vector n a
(<^>) x (Vector xs) = Vector $ V.map (*x) xs

-- ** Useful combinators for manipulating tensors
-- | Map operation
mapV :: KnownNat n => (a -> b) -> Vector n a -> Vector n b
mapV f (Vector v) = Vector $ V.map f v

-- | Left fold
foldlV :: KnownNat n => (a -> b -> a) -> a -> Vector n b -> a
foldlV f z (Vector v) = V.foldl f z v

-- | Right fold
foldrV :: KnownNat n => (a -> b -> b) -> b -> Vector n a -> b
foldrV f z (Vector v) = V.foldr f z v

-- | ZipWith
zipWithV :: KnownNat n => (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
zipWithV f (Vector v) (Vector v') =Vector $ V.zipWith f v v'
