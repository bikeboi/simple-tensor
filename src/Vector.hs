{-# LANGUAGE DataKinds, BangPatterns, KindSignatures #-}

module Vector where

import GHC.TypeLits
import Data.Proxy
-- Underlying vectos
import qualified Data.Vector as V

-- Test implementation using regular lists
newtype Vector (n :: Nat) a =
  Vector (V.Vector a)
  deriving Show

unsafeTagVector :: KnownNat n => Proxy n -> [a] -> Vector n a
unsafeTagVector _ xs = Vector $ V.fromList xs

-- Smart constructors
genVector :: KnownNat n => Proxy n -> (Integer -> a) -> Vector n a  
genVector p f = Vector $ V.fromList $ map f [1 .. natVal p] 

(|>) :: (Num a, KnownNat n) => Proxy n -> [a] -> Vector n a
(|>) p xs = let xs' = take expLen xs
                expLen = fromInteger $ natVal p
            in unsafeTagVector p (fill expLen xs')
  where fill x as = let len = length as
                    in if len < x
                       then as ++ replicate (x - len) 0
                       else as

-- Simple vector operations
(<+>) :: Num a => Vector m a -> Vector m a -> Vector m a
(<+>) (Vector xs) (Vector ys) = Vector $ V.zipWith (+) xs ys

scal :: Num a => a -> Vector n a -> Vector n a
scal x (Vector xs) = Vector $ V.map (*x) xs

-- Usual vector operations lifted into the newtype wrapper
mapV :: KnownNat n => (a -> b) -> Vector n a -> Vector n b
mapV f (Vector v) = Vector $ V.map f v

foldlV :: KnownNat n => (a -> b -> a) -> a -> Vector n b -> a
foldlV f z (Vector v) = V.foldl f z v

-- Test Vecs
type IVec n = Vector n Int 
vec3 = (Proxy :: Proxy 3) |> [0..] :: IVec 3
vec4 = (Proxy :: Proxy 4) |> [3..] :: IVec 4
vec5 = (Proxy :: Proxy 5) |> [2..] :: IVec 5

-- Matrices
newtype Matrix (r :: Nat) (c :: Nat) a =
  Matrix (V.Vector (V.Vector a))

instance Show a => Show (Matrix r c a) where
  show (Matrix rows) =
        V.foldl (\a b -> a ++ show b ++ "\n") "" rows

genMatrix :: (KnownNat row,KnownNat col) => Proxy row -> Proxy col -> (Int -> Int -> a) -> Matrix row col a
genMatrix pr pc f = let (nRows,nCols) = (,) (fromInteger $ natVal pr) (fromInteger $ natVal pc)
                        rows = V.generate nRows (\ri -> V.generate nCols (\ci -> f ri ci))
                    in Matrix rows

(|+|) :: Num a => Matrix r c a -> Matrix r c a -> Matrix r c a
(|+|) (Matrix v) (Matrix v') = Matrix $ V.zipWith (addBoxed) v v'
  where addBoxed :: Num a => V.Vector a -> V.Vector a -> V.Vector a
        addBoxed = V.zipWith (+)

(|*>) :: (KnownNat r, KnownNat c,Num a) => Matrix r c a -> Vector c a -> Vector r a
(|*>) (Matrix rows) (Vector v) = Vector $ V.map (\row -> V.foldl (+) 0 $ V.zipWith (*) row v) rows

-- Test Matrices
m3x4 = genMatrix (Proxy :: Proxy 3) (Proxy :: Proxy 4) (+)
m3x4' = genMatrix (Proxy :: Proxy 3) (Proxy :: Proxy 4) const
