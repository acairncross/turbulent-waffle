import Vector3
import Data.Array
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.List (intercalate, minimumBy)
import Control.Monad (liftM, liftM2, sequence)

type Point = Vector3
type Direction = Vector3
type Radius = Double
type Sphere = (Point,Radius)
type PointLight = Point
type Ray = (Point,Direction)
type Intersection = (Point,Direction)
type ReflIntersection = (PointLight,Intersection)

width = 500 :: Int
height = 500 :: Int

spheres :: [Sphere]
spheres = [((-25, -25, 380), 30),
           ((100, 0, 500), 80),
           ((-50, 150, 300), 50),
           ((-100, -100, 700), 100),
           ((-100, -100, 500), 15)]

α = 3 :: Double

lights :: [Point]
lights = [(-100, 0, 250), (500, 100, 1000)]

raytrace :: Ray -> Double
raytrace (o,l) =
  let intersection = (intersect (o,l) spheres)
      reflIntersect light =
        (\(p,n) -> let shadowRay = (p,normalize (light⊖p))
                       shadowIntersection = intersect shadowRay spheres
                   in  case shadowIntersection of
                              Nothing -> Just (light,(p,n))
                              Just _ -> Nothing)
      intensity (light,(p,n)) =
        let ŀ = normalize (light⊖p)
            ṙ = ((2*(ŀ⋅n)) `scale` n) ⊖ ŀ
        in  (ŀ⋅n) + (ṙ⋅(neg l))**α + raytrace (p,ṙ)
  in  sum (map ((fromMaybe 0) . (liftM intensity))
               (sequence (map ((=<<) . reflIntersect) lights) intersection))

intersect :: Ray -> [Sphere] -> Maybe Intersection
intersect (o,l) [] = Nothing
intersect (o,l) ss =
  let (c,r) = minimumBy (maybeCompare `on` (intersectSphere (o,l))) ss
      distToIntersection = (\d -> let point = o⊕(d `scale` l)
                                  in  (point, normalize (point⊖c)))
  in  fmap distToIntersection (intersectSphere (o,l) (c,r))

-- Nothing is max instead of min
maybeCompare :: Ord a => Maybe a -> Maybe a -> Ordering
(Just x) `maybeCompare` (Just y) = x `compare` y
(Just x) `maybeCompare` Nothing = LT
Nothing `maybeCompare` (Just x) = GT
Nothing `maybeCompare` Nothing = EQ

intersectSphere :: Ray -> Sphere -> Maybe Double
intersectSphere (o,l) (c,r)
  | discriminant > 0 && dMinus > 0.001 = Just dMinus
  | discriminant > 0 && dPlus > 0.001 = Just dPlus
  | otherwise = Nothing
  where discriminant = (l⋅(o⊖c))**2 - ((o⊖c)⋅(o⊖c)) + r**2
        dMinus = -(l⋅(o⊖c)) - sqrt discriminant
        dPlus = -(l⋅(o⊖c)) + sqrt discriminant

imageToWorld :: (Int,Int) -> Vector3
imageToWorld (i,j) = ((fromIntegral j) - (fromIntegral width)/2,
                      (fromIntegral height)/2 - (fromIntegral i),
                      0)

image :: [[Int]]
image = let matmax xss = maximum [(maximum xs) | xs <- xss]
            matscale c xss  = [[c*x | x <- xs] | xs <- xss]
            matfloor xss = [[floor x | x <- xs] | xs <- xss]
            mat = [[ raytrace (imageToWorld (i,j), (0,0,1))
                   | j <- [0..width-1] ]
                   | i <- [0..height-1] ]
        in  matfloor $ matscale (255/(matmax mat)) mat

main = do
  putStrLn "P2"
  putStrLn $ intercalate " " $ map show [width, height]
  putStrLn $ "255"
  putStrLn $ intercalate "\n" $ [intercalate " " $ map show row | row <- image]
