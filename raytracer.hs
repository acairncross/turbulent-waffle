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

width = 500 :: Int
height = 500 :: Int

surfaces :: [Surface]
surfaces = [Sphere (-100,0,200) 100, Plane (0,-40,0) (0,1,0)]

α = 100 :: Double

lights :: [Point]
lights = [(200,300,0)]

raytrace :: Ray -> Double
raytrace (o,l) =
  let intersection = (intersect (o,l) surfaces)
      reflIntersect light =
        (\(p,n) -> let shadowRay = (p,normalize (light⊖p))
                       shadowIntersection = intersect shadowRay surfaces
                   in  case shadowIntersection of
                              Nothing -> Just (light,(p,n))
                              Just _ -> Nothing)
      intensity (light,(p,n)) =
        let ŀ = normalize (light⊖p)
            ṙ = ((2*(ŀ⋅n)) `scale` n) ⊖ ŀ
        in  (ŀ⋅n) + (ṙ⋅(neg l))**α + raytrace (p,ṙ)
  in  sum (map ((fromMaybe 0) . (liftM intensity))
               (sequence (map ((=<<) . reflIntersect) lights) intersection))

intersect :: Ray -> [Surface] -> Maybe Intersection
intersect (o,l) [] = Nothing
intersect (o,l) ss =
  let s = minimumBy (maybeCompare `on` (intersectSurface (o,l))) ss
      distToIntersection = (\d -> let point = o⊕(d `scale` l)
                                  in  case s of
                                        Sphere c r -> (point, normalize (point⊖c))
                                        Plane p n -> (point, n))
  in  fmap distToIntersection (intersectSurface (o,l) s)

-- Nothing is max instead of min
maybeCompare :: Ord a => Maybe a -> Maybe a -> Ordering
(Just x) `maybeCompare` (Just y) = x `compare` y
(Just x) `maybeCompare` Nothing = LT
Nothing `maybeCompare` (Just x) = GT
Nothing `maybeCompare` Nothing = EQ

intersectSurface :: Ray -> Surface -> Maybe Double
intersectSurface (o,l) (Sphere c r)
  | discriminant > 0 && dMinus > 0.001 = Just dMinus
  | discriminant > 0 && dPlus > 0.001 = Just dPlus
  | otherwise = Nothing
  where discriminant = (l⋅(o⊖c))**2 - ((o⊖c)⋅(o⊖c)) + r**2
        dMinus = -(l⋅(o⊖c)) - sqrt discriminant
        dPlus = -(l⋅(o⊖c)) + sqrt discriminant
intersectSurface (o,l) (Plane p n) =
  let d = ((p⊖l)⋅n)/(l⋅n)
  in if abs (l⋅n) == 0 || d < 0.001 then Nothing else Just d


imageToWorld :: (Int,Int) -> Vector3
imageToWorld (i,j) = ((fromIntegral j) - (fromIntegral width)/2,
                      (fromIntegral height)/2 - (fromIntegral i),
                      0)

image :: [[Int]]
image = let matmax xss = maximum [(maximum xs) | xs <- xss]
            matscale c xss  = [[c*x | x <- xs] | xs <- xss]
            matfloor xss = [[floor x | x <- xs] | xs <- xss]
            mat = [[ raytrace (imageToWorld (i,j), normalize ((imageToWorld (i,j))⊖(0,0,-1000)))
                   | j <- [0..width-1] ]
                   | i <- [0..height-1] ]
        in  matfloor $ matscale (255/(matmax mat)) mat

main = do
  putStrLn "P2"
  putStrLn $ intercalate " " $ map show [width, height]
  putStrLn $ "255"
  putStrLn $ intercalate "\n" $ [intercalate " " $ map show row | row <- image]
