{-# LANGUAGE DataKinds, KindSignatures #-}

module Data.Simple.Matrix where

import GHC.TypeLits
import Data.Proxy
import qualified Data.Vector as V
import qualified Data.Simple.Vector as SV

newtype Matrix (r :: Nat) (c :: Nat) a =
  Matrix (V.Vector (V.Vector a))

instance Show a => Show (Matrix r c a) where
  show (Matrix rows) =
        V.foldl (\a b -> a ++ show b ++ "\n") "" rows

mkMatrix :: (KnownNat row,KnownNat col)
          => Proxy row -> Proxy col
          -> (Int -> Int -> a)
          -> Matrix row col a
mkMatrix pr pc f = let (nRows,nCols) = toBoth fromInteger (natVal pr, natVal pc)
                       rows = V.generate nRows (\ri -> V.generate nCols
                                                 (\ci -> f ri ci))
                   in Matrix rows
  where toBoth f (a,a') = (f a,f a')

(|+|) :: Num a => Matrix r c a -> Matrix r c a -> Matrix r c a
(|+|) (Matrix v) (Matrix v') = Matrix $ V.zipWith (addBoxed) v v'
  where addBoxed = V.zipWith (+)

(|*>) :: (KnownNat r, KnownNat c,Num a) => Matrix r c a -> SV.Vector c a -> SV.Vector r a
(|*>) (Matrix rows) (SV.Vector v) = SV.Vector
                                    $ V.map (\row ->
                                               V.foldl (+) 0 $ V.zipWith (*) row v) rows

transpose :: (KnownNat r, KnownNat c) => Matrix r c a -> Matrix c r a
transpose (Matrix v) = Matrix $ V.foldl1 (V.zipWith (<>)) v'
  where v' = fmap V.singleton <$> v
 
