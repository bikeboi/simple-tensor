{-# LANGUAGE DataKinds, KindSignatures, FlexibleInstances #-}

module Data.Simple.Vector where

import Prelude hiding (foldl,map)
import Unsafe.Coerce
import GHC.TypeLits
import Data.Proxy
import Data.Reflection
import qualified Data.Vector as V

-- | Vector type, row or column
data VecType = Row | Col

-- | General tagged vector
newtype Vector (t :: VecType) (n :: Nat) a =
  Vector (V.Vector a) deriving (Eq, Show)

type CVector n a = Vector Col n a
type RVector n a = Vector Row n a

-- | Yield a pure unsized vector from it's tagged wrapper.
-- Use this sparingly
unsafeTagVector :: KnownNat n => Proxy n -> [a] -> Vector t n a
unsafeTagVector _ xs = Vector $ V.fromList xs

-- Smart constructors
-- | Make vector given a known length and a generation function
mkVector :: KnownNat n => Proxy n -> (Integer -> a) -> Vector t n a  
mkVector p f = Vector $ V.fromList $ fmap f [1 .. natVal p] 

-- | Make a vector given a known length, a default value and a list.
-- If the list is too long, it gets truncated. If it's too short,
-- the remaining length is filled with the default value.
fromList :: KnownNat n => a -> Proxy n -> ([a] -> Vector t n a)
fromList d p = \xs -> let xs' = take expLen xs
                          expLen = fromInteger $ natVal p
                      in unsafeTagVector p (fill expLen xs')
  where fill x as = let len = length as
                    in if len < x
                       then as ++ replicate (x - len) d else as

-- | Specialized version of 'fromList' for 'Num' types. The default
-- value is always 0
(|>) :: (Num a , KnownNat n) => Proxy n -> [a] -> Vector t n a
(|>) = fromList 0

-- **Vector operations
-- | Addition
(<+>) :: Num a => Vector t m a -> Vector t m a -> Vector t m a
(<+>) (Vector xs) (Vector ys) = Vector $ V.zipWith (+) xs ys

-- | Subtraction
(<->) :: Num a => Vector t n a -> Vector t n a -> Vector t n a
(<->) (Vector v) (Vector v') = Vector $ V.zipWith (-) v v'

-- | Dot product
(<.>) :: Num a => Vector t n a -> Vector t n a -> a
(<.>) (Vector v) (Vector v') = V.sum $ V.zipWith (*) v v' 

-- | Scalar multiplication
(^>) :: Num a => a -> Vector t n a -> Vector t n a
(^>) x (Vector xs) = Vector $ V.map (*x) xs

-- ** Norms
euclidean :: (Real a,Floating b) => Vector t n a -> b 
euclidean = sqrt . foldl (+) 0 . map (realToFrac . (^2))

manhattan :: (Real a) => Vector t n a -> a
manhattan = foldl (+) 0 . map abs

-- ** Useful combinators for data-structure-like operations on vectors
-- | Vector size
size :: KnownNat n => Vector t n a -> Integer
size = natVal . sizeProxy
  where sizeProxy :: Vector t n a -> Proxy n
        sizeProxy _ = Proxy

-- | Map operation
map :: (a -> b) -> Vector t n a -> Vector t n b
map f (Vector v) = Vector $ V.map f v

-- | Left fold
foldl :: (a -> b -> a) -> a -> Vector t n b -> a
foldl f z (Vector v) = V.foldl f z v

-- | Right fold
foldr :: (a -> b -> b) -> b -> Vector t n a -> b
foldr f z (Vector v) = V.foldr f z v

-- | ZipWith
zipWith :: KnownNat n => (a -> b -> c) -> Vector t n a -> Vector t n b -> Vector t n c
zipWith f (Vector v) (Vector v') = Vector $ V.zipWith f v v'
