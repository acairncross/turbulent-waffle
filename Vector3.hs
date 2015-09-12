{-# LANGUAGE UnicodeSyntax #-}

module Vector3
( Vector3
, (⊕)
, (⊖)
, (⋅)
, translate
, scale
, neg
, mag
, normalize
) where

type Vector3 = (Double,Double,Double)

infixl 5 ⊕
(⊕) :: Vector3 -> Vector3 -> Vector3
(x1,y1,z1) ⊕ (x2,y2,z2) = (x1+x2,y1+y2, z1+z2)

infixl 5 ⊖
(⊖) :: Vector3 -> Vector3 -> Vector3
(x1,y1,z1) ⊖ (x2,y2,z2) = (x1-x2,y1-y2, z1-z2)

infixl 7 ⋅
(⋅) :: Vector3 -> Vector3 -> Double
(⋅) (x1,y1,z1) (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2

translate :: Double -> Vector3 -> Vector3
translate c (x,y,z) = (c+x,c+y,c+z)

scale :: Double -> Vector3 -> Vector3
scale c (x,y,z) = (c*x,c*y,c*z)

neg :: Vector3 -> Vector3
neg (x,y,z) = (-x,-y,-z)

mag :: Vector3 -> Double
mag u = sqrt $ u⋅u

normalize :: Vector3 -> Vector3
normalize u = scale (1/(mag u)) u
