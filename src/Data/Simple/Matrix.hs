{-# LANGUAGE DataKinds, KindSignatures #-}

module Data.Simple.Matrix where

import GHC.TypeLits
import Data.Proxy
import qualified Data.Vector as V
import qualified Data.Simple.Vector as SV

-- | Matrix datatype. Newtype wrapper over Boxed Vector of boxed vectors
newtype Matrix (r :: Nat) (c :: Nat) a =
  Matrix (V.Vector (V.Vector a))

-- | A little pretty printing
instance Show a => Show (Matrix r c a) where
  show (Matrix rows) =
        V.foldl (\a b -> a ++ show b ++ "\n") "" rows

-- | Construct a matrix with dimensions specified in the proxy types
mkMatrix :: (KnownNat row,KnownNat col)
          => Proxy row -> Proxy col
          -> (Int -> Int -> a)
          -> Matrix row col a
mkMatrix pr pc f = let (nRows,nCols) = toBoth fromInteger (natVal pr, natVal pc)
                       rows = V.generate nRows (\ri -> V.generate nCols
                                                 (\ci -> f ri ci))
                   in Matrix rows
  where toBoth f (a,a') = (f a,f a')

-- Matrix addition
(|+|) :: Num a => Matrix r c a -> Matrix r c a -> Matrix r c a
(|+|) (Matrix v) (Matrix v') = Matrix $ V.zipWith (addBoxed) v v'
  where addBoxed = V.zipWith (+)

-- | Matrix - Vector multiplication
(|*>) :: (KnownNat r, KnownNat c,Num a) => Matrix r c a -> SV.CVector c a -> SV.CVector r a
(|*>) (Matrix rows) (SV.Vector v) = SV.Vector $ f rows v
  where f rows v = V.map (\row -> V.foldl (+) 0 $ V.zipWith (*) row v) rows

-- | Matrix - Matrix multiplication
(|*|) :: (KnownNat r, KnownNat c, Num a) => Matrix r c a -> Matrix c c' a -> Matrix r c' a
(|*|) m1 m2 = undefined

-- | Matrix transposition
transpose :: (KnownNat r, KnownNat c) => Matrix r c a -> Matrix c r a
transpose (Matrix v) = Matrix $ V.foldl1 (V.zipWith (<>)) v'
  where v' = fmap V.singleton <$> v
 
