import Vector3
import Data.Array
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.List (intercalate, minimumBy)
import Control.Monad (liftM, liftM2, sequence)

type Radius = Double
type Point = Vector3
type Direction = Vector3
type PointLight = Point
type Ray = (Point,Direction)
type Intersection = (Point,Direction)
type ReflIntersection = (PointLight,Intersection)

data Surface = Sphere Point Radius | Plane Point Direction

α = 100 :: Double

width = 500 :: Int
height = 500 :: Int

surfaces :: [Surface]
surfaces = [Sphere (-100,-100,300) 100,
            Sphere (150,-100,450) 50,
            Plane (0,-50,0) (0,1,0)]

lights :: [Point]
lights = [(200,300,0), (400, 0, 50)]

raytrace :: Int -> Ray -> Double
raytrace depth (o,l) =
  let intersection = intersect (o,l) surfaces
      reflIntersect light =
        (\(p,n) ->
            let shadowRay = (p,normalize (light⊖p))
                shadowIntersection = intersect shadowRay surfaces
                reflIntersection = (light,(p,n))
            in  case shadowIntersection of
                      Nothing -> Just reflIntersection
                      Just (p',n') -> if mag (p'⊖p) < mag (light⊖p)
                                          then Nothing
                                          else Just reflIntersection)
      intensity (light,(p,n)) =
        let l' = normalize (light⊖p)
            n' = n
            r' = ((2*(l'⋅n')) `scale` n') ⊖ l'
            v' = neg l
        in  (l'⋅n') + (if depth < 3 then raytrace (depth+1) (p,r') else 0)*(r'⋅v')**α
  in sum (map (maybe 0 intensity)
      (sequence (map ((=<<) . reflIntersect) lights) intersection))

intersect :: Ray -> [Surface] -> Maybe Intersection
intersect (o,l) [] = Nothing
intersect (o,l) ss =
  let s = minimumBy (maybeCompare `on` (intersectSurface (o,l))) ss
      distToIntersection = (\d -> let point = o⊕(d `scale` l)
                                  in  (point,normal point s))
  in  fmap distToIntersection (intersectSurface (o,l) s)

-- Nothing is max instead of min
maybeCompare :: Ord a => Maybe a -> Maybe a -> Ordering
(Just x) `maybeCompare` (Just y) = x `compare` y
(Just x) `maybeCompare` Nothing = LT
Nothing `maybeCompare` (Just x) = GT
Nothing `maybeCompare` Nothing = EQ

-- Closest (in forwards direction) part of the surface that intersects, if any
intersectSurface :: Ray -> Surface -> Maybe Double
intersectSurface (o,l) (Sphere c r)
  | discriminant > 0 && dMinus > 0.001 = Just dMinus
  | discriminant > 0 && dPlus > 0.001 = Just dPlus
  | otherwise = Nothing
  where discriminant = (l⋅(o⊖c))**2 - ((o⊖c)⋅(o⊖c)) + r**2
        dMinus = -(l⋅(o⊖c)) - sqrt discriminant
        dPlus = -(l⋅(o⊖c)) + sqrt discriminant
intersectSurface (o,l) (Plane p n)
  | abs (l⋅n) /= 0 && d > 0.001 = Just d
  | otherwise = Nothing
  where d = ((p⊖l)⋅n)/(l⋅n)

imageToWorld :: (Int,Int) -> Vector3
imageToWorld (i,j) = ((fromIntegral j) - (fromIntegral width)/2,
                      (fromIntegral height)/2 - (fromIntegral i),
                      0)

normal :: Point -> Surface -> Direction
normal p (Sphere c r) = normalize $ p⊖c
normal _ (Plane p n) = n

image :: [[Int]]
image = let matmax xss = maximum [(maximum xs) | xs <- xss]
            matscale c xss  = [[c*x | x <- xs] | xs <- xss]
            matfloor xss = [[floor x | x <- xs] | xs <- xss]
            matclamp n xss = [[min n x | x <- xs] | xs <- xss]
            mat = [[ raytrace 0 (imageToWorld (i,j), normalize ((imageToWorld (i,j))⊖(0,0,-1000)))
                   | j <- [0..width-1] ]
                   | i <- [0..height-1] ]
        in  matclamp 255 $ matfloor $ matscale (2*255/(matmax mat)) mat

main = do
  putStrLn "P2"
  putStrLn $ intercalate " " $ map show [width, height]
  putStrLn $ "255"
  putStrLn $ intercalate "\n" $ [intercalate " " $ map show row | row <- image]
