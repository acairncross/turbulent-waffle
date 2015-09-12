import Vector3
import Data.Array
import Data.Function (on)
import Data.List (intercalate, minimumBy)
import Text.Printf (printf)

type Point = Vector3
type Direction = Vector3
type Radius = Double
type Sphere = (Point,Double)
type Ray = (Point,Direction)
type ShadowRay = (Ray,Sphere)

width = 500 :: Int
height = 500 :: Int

spheres :: [Sphere]
spheres = [((0, 0, 380), 30),
           ((100, 0, 500), 80),
           ((-50, 150, 300), 50),
           ((-100, -100, 700), 100),
           ((-100, -100, 500), 15)]

light :: Vector3
light = (-100, 0, 250)

traceRay :: Ray -> Maybe Double
traceRay (o,l) =
  let shadowRay (s,d) = let o2 = o⊕(scale d l)
                            l2 = normalize (light⊖o2)
                        in  ((o2,l2),s)
      intensity ((o2,l2),(c,r)) =
        maybe (l2⋅(normalize $ o2⊖c)) (const 0) (intersect (o2,l2) spheres)
  in  fmap (intensity . shadowRay) (intersect (o,l) spheres)

intersect :: Ray -> [Sphere] -> Maybe (Sphere,Double)
intersect ray [] = Nothing
intersect ray ss =
  let s = minimumBy (maybeCompare `on` (intersectSphere ray)) ss
  in  fmap (\d -> (s,d)) (intersectSphere ray s)

-- Like the derived 'compare' for Maybe, but with Nothing as the largest
-- instead of the smallest.
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
image = [[ (toPixel . traceRay) (imageToWorld (i,j), (0,0,1))
           | j <- [0..width-1] ]
           | i <- [0..height-1] ]
          where toPixel = maybe 0 (\x -> floor (255 * x))

main = do
  putStrLn "P2"
  putStrLn $ intercalate " " $ map show [width, height]
  putStrLn $ "255"
  putStrLn $ intercalate "\n" $ [intercalate " " $ map show row | row <- image]
