{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

module Main (main) where

import Codec.Picture
import Control.Monad
import Control.Monad.ST
import Data.Fixed
import System.Environment (getArgs)
import System.FilePath (replaceExtension)

--data ImgFormat = Bmp | Jpg | Png | Tiff

main :: IO ()
main = do
  [path] <- getArgs
  -- Here we should parse arguements, then scene file
  savePngImage path generateImg

imgWidth = 200
imgHeight = 150

generateImg :: DynamicImage
generateImg = ImageRGB8 (generateImage colorPixel imgWidth imgHeight)

-- Scene (would be nice to use actual types for different objects)

background = [0.06,0.08,0.12]

cameraPos = [0,0,-2]

spherePos = [0,0,0]
sphereColor = [0.9,0.95,1]

planeHeight = -0.6

lightPos = [2.2, 10.0, -10.6]
lightAmp = 1.0

-- Intersection and lighting parameters
escapeDistance = 100.0
escapeFlag = [-999,-999,-999]
intersectThreshold = 0.005
eps = 0.00001
stepSize = 0.2--0.998
maxSteps = 500
--maxBounces = 4

fov = 1.0

colorPixel :: Int -> Int -> PixelRGB8
colorPixel x y = PixelRGB8 (toPix (col!!0))
                           (toPix (col!!1))
                           (toPix (col!!2))
                           where col = colorThroughPixel x y
                                 toPix x = round (min (255.0 * x) 255.0)

colorThroughPixel :: Int -> Int -> [Double]
colorThroughPixel x y = 
  radiance cameraPos (normalize $
  (normUV x (imgHeight - y) 
  (fromIntegral imgWidth) (fromIntegral imgHeight)) ++ [fov])

normCoord v m = -1.0 + 2.0 * (v / m) 

normUV :: Int -> Int -> Double -> Double -> [Double]
normUV x y xdim ydim = [(xdim/ydim)*(normCoord (fromIntegral x) xdim), 
                                     normCoord (fromIntegral y) ydim]

radiance :: [Double] -> [Double] -> [Double]
radiance origin direction = 
  if i == escapeFlag
    then background
    else vecMulScalar (vecMulScalar sphereColor $ 
      lightAmp * max (dot (sceneNormal i)
      (lightDir i)) 0.0) (shadowAtten i (lightDir i))
      where i = intersect origin direction

shadowAtten :: [Double] -> [Double] -> Double
shadowAtten origin direction = 
  if (intersect p direction) == escapeFlag
    then 1.0
    else 0.05
    where p = vecAdd origin (vecMulScalar direction 0.05)

lightAtten :: [Double] -> Double
lightAtten v = 1.0 / distance v lightPos 

intersect :: [Double] -> [Double] -> [Double]
intersect origin direction = intersect' origin (normalize direction) 0
    where 
  intersect' origin direction step
      | intersectThreshold < d &&
        d < escapeDistance && 
        step <= maxSteps = 
          intersect' (vecAdd origin $ vecMulScalar 
          direction $ stepSize * d ) direction (step+1)
      | d < intersectThreshold = origin
      | otherwise = escapeFlag
      where d = sceneSDF origin

sceneSDF :: [Double] -> Double
sceneSDF v = minimum $ [ wildSDF v [x,z*0.5,z] (3*am*(z+1)) (0.15*am) 0.3 | 
  x <- [-wid..wid], z <- [0..5], let am = wid - (abs x) ] ++ [planeSDF v]
  where wid = 2

planeSDF :: [Double] -> Double
planeSDF v = dot v [0,1,0] - planeHeight

wildSDF :: [Double] -> [Double] -> Double -> Double -> Double -> Double
wildSDF v offset freq amp radius = 
  distance (p `vecAdd` ( (vecSin (p `vecMulScalar` 
  freq) ) `vecMulScalar` amp ))
  spherePos - radius
  where p = vecSub v offset

sceneNormal :: [Double] -> [Double]
sceneNormal v = normalize [partialDeriv v [1,0,0], 
                           partialDeriv v [0,1,0], 
                           partialDeriv v [0,0,1]]

partialDeriv :: [Double] -> [Double] -> Double
partialDeriv v dir = (sceneSDF v) -
  (sceneSDF (vecSub v (vecMulScalar dir eps)))

lightDir :: [Double] -> [Double]
lightDir v = normalize (vecSub lightPos v)

-- vector operations - using haskell lists (inefficient but fun)

vecOpp :: Num a => [a] -> [a] -> (a -> a -> a) -> [a] -- should "a" be a Double?
vecOpp v1 v2 op = zipWith op v1 v2

vecScalarOpp :: Num a => [a] -> a -> (a -> a -> a) -> [a]
vecScalarOpp v s op = map (`op` s) v

-- between a vector and a scalar

vecAddScalar :: [Double] -> Double -> [Double]
vecAddScalar v s = vecScalarOpp v s (+) 

vecMulScalar :: [Double] -> Double -> [Double]
vecMulScalar v s = vecScalarOpp v s (*)

vecDivScalar :: [Double] -> Double -> [Double]
vecDivScalar v s = vecScalarOpp v s (/)

vecMod :: [Double] -> Double -> [Double]
vecMod v n = vecScalarOpp v n mod'

-- between two vectors

vecAdd :: [Double] -> [Double] -> [Double]
vecAdd v1 v2 = vecOpp v1 v2 (+)

vecSub :: [Double] -> [Double] -> [Double]
vecSub v1 v2 = vecOpp v1 v2 (-)

vecMul :: [Double] -> [Double] -> [Double]
vecMul v1 v2 = vecOpp v1 v2 (*)

--

l2Norm :: [Double] -> Double
l2Norm v = sqrt $ sum $ map (\x -> x*x) v

distance :: [Double] -> [Double] -> Double
distance v1 v2 = l2Norm $ vecSub v1 v2

normalize :: [Double] -> [Double]
normalize v = vecDivScalar v $ l2Norm v

dot :: [Double] -> [Double] -> Double
dot v1 v2 = sum $ map (\(a,b) -> a*b) $ zip v1 v2

reflect :: [Double] -> [Double] -> [Double]
reflect v n = v `vecSub` ( n `vecMulScalar` ((dot n v) * 2.0) )

vecSin :: [Double] -> [Double]
vecSin v = map (\x -> sin x) v

--
