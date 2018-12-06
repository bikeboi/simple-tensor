{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, KindSignatures, ConstraintKinds, PolyKinds, UndecidableInstances #-}

module Vec where

data Nat = Z | S Nat
data Boolean = T | F

type family n :=: m :: Boolean where
  Z :=: Z = T
  S n :=: S n = n :=: n
  x :=: y = F

type family n :+: m :: Nat where
  Z :+: Z = Z
  S n :+: Z = S n
  Z :+: S n = S n
  S n :+: S m = S (S n) :+: m

-- Test implementation using regular lists
newtype Vec (n :: Nat) a =
  Vec [a] -- ^ Unsafe, we will not export
  deriving Show

type Vec1 = Vec (S Z)
type Vec2 = Vec (S (S Z))
type Vec3 = Vec (S (S (S Z)))

-- Simple vector operations
(|+|) :: (Num a, m ~ n) => Vec m a -> Vec n a -> Vec n a
(|+|) (Vec xs) (Vec ys) = Vec $ zipWith (+) xs ys

scalar :: Num a => a -> Vec n a -> Vec n a
scalar x (Vec xs) = Vec $ map (*x) xs

-- Smart constructors
vec2 :: a -> a -> Vec2 a
vec2 x x' = Vec [x,x']

vec3 :: a -> a -> a -> Vec3 a
vec3 x x' x'' = Vec [x,x',x'']

-- Samples
testVec2 = vec2 1 2
testVec2' = vec2 3 4
testVec3 = vec3 1 2 3
