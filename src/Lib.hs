{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Lib
    ( Option(..),
    defaultOption,
    initValue,
    dataPixel,
    Pixel(..),
    distance,
    Cluster(..),
    createNewCluster
    ) where
import System.Random
import Control.Monad (replicateM)
import System.Environment
import Data.Maybe
import Data.List
import System.Exit
import Text.Read

type Truple = (Int, Int, Int)
type Pos = (Int, Int)
type Cluster = (Truple, [Pixel])

data Pixel = Pixel {
    position :: Pos,
    color :: Truple
} deriving Show

data Option = Option {
    nbCluster :: Int,
    convLim :: Float,
    pathToFile :: String
} deriving (Show)

defaultOption :: Option
defaultOption = Option {
    nbCluster = 0,
    convLim = 0,
    pathToFile = ""
}

distance :: Truple -> Truple -> Float
distance (xa,ya,za) (xb,yb,zb) = sqrt $ fromIntegral $
        (xa - xb) ^ 2 + (ya - yb) ^ 2 + (za - zb) ^ 2

dataPixel :: [String] -> Pixel
dataPixel [a,b] = Pixel {position=read a, color=read b}

dataArrayPixel :: [String] -> [Pixel]
dataArrayPixel [x] = [dataPixel (words x)]
dataArrayPixel ("":xs) = dataArrayPixel xs
dataArrayPixel (x:xs) = dataPixel (words x):dataArrayPixel xs

verifyFiles :: String -> IO ()
verifyFiles a | null a = exitWith (ExitFailure 84)
              | otherwise = return ()

readFileToPixels :: String -> IO [Pixel]
readFileToPixels file = do
                pixels <- readFile file
                verifyFiles pixels
                return (dataArrayPixel (lines pixels))

createNewCluster :: Truple -> Cluster
createNewCluster (r,g,b) = ((r,g,b), [])

randomColor :: IO Truple
randomColor = do
    r <- randomRIO (0, 255)
    g <- randomRIO (0, 255)
    b <- randomRIO (0, 255)
    return (r,g,b)

fillCluster :: Int -> IO [Cluster]
fillCluster 0 = return []
fillCluster n = do
            ncolor <- randomColor
            ncluster <- fillCluster (n - 1)
            return (createNewCluster ncolor:ncluster)

getNearliestCluster :: Pixel -> [Cluster] -> Float -> (Int, Int) -> Int
getNearliestCluster _ [] _ (_, best) = best
getNearliestCluster pix@Pixel {color=rgb} ((rgb2, _):xs) minDist (curr, best)
    | dist < minDist = getNearliestCluster pix xs dist (curr + 1, curr)
    | otherwise = getNearliestCluster pix xs minDist (curr + 1, best)
    where dist = distance rgb rgb2

putPixelInCluster :: Pixel -> Int -> [Cluster] -> [Cluster]
putPixelInCluster a 0 ((rgb, arr):xs) = (rgb, a:arr):xs
putPixelInCluster a nb (x:xs) = x:putPixelInCluster a (nb - 1) xs

fillPixelsInCluster :: [Pixel] -> [Cluster] -> [Cluster]
fillPixelsInCluster [] a = a
fillPixelsInCluster (x:xs) a = fillPixelsInCluster xs filledCluster
    where bestidx = getNearliestCluster x a 500 (0, 0)
          filledCluster = putPixelInCluster x bestidx a

averageColor :: [Pixel] -> Truple -> Int -> Truple
averageColor [] _ 0 = (0,0,0)
averageColor [] (r,g,b) nb = (r `div` nb, g `div` nb, b `div` nb)
averageColor (Pixel {color=(r1,g1,b1)}:xs) (r2,g2,b2) nb =
        averageColor xs (r1 + r2, g1 + g2, b1 + b2) (nb + 1)

getNewClusterColor :: [Cluster] -> [Cluster]
getNewClusterColor [] = []
getNewClusterColor ((_, pixs):xs) =
        (averageColor pixs avrColor nb, []):getNewClusterColor xs
        where nb = 0
              avrColor = (0,0,0)

getMoveCluster :: [Cluster] -> [Cluster] -> Float -> Bool
getMoveCluster [] [] _ = False
getMoveCluster ((x1, _):xs1) ((x2, _):xs2) conv
    | distance x1 x2 < conv = True
    | otherwise = getMoveCluster xs1 xs2 conv

printCluster :: [Cluster] -> IO ()
printCluster [] = return ()
printCluster ((x,b):xs) = putStrLn ("--\n" ++ show x ++  "\n-\n") >>
                            printClusterPixel b >> printCluster xs

printClusterPixel :: [Pixel] -> IO ()
printClusterPixel [] = return ()
printClusterPixel ((Pixel a b):xs) = putStr(show a) >> putChar ' ' >>
                                    print b >> printClusterPixel xs

loopGame :: [Cluster] -> [Pixel] -> Float -> [Cluster]
loopGame nc pix conv
    | getMoveCluster nc newCluster conv =
        fillPixelsInCluster pix newCluster
    | otherwise =  loopGame newCluster pix conv
    where newCluster = getNewClusterColor (fillPixelsInCluster pix nc)

-- initValue :: Option -> IO ()
-- initValue Option {nbCluster=c, convLim=conv, pathToFile=filepath} = do
--                     allCluster <- fillCluster c
--                     allPixels <- readFileToPixels filepath
--                     loopGame allCluster allPixels conv

initValue :: Option -> IO ()
initValue Option {nbCluster=c, convLim=conv, pathToFile=filepath} =
                    fillCluster c >>= \allCluster ->
                    readFileToPixels filepath >>= \allPixels ->
                    printCluster $ loopGame allCluster allPixels conv