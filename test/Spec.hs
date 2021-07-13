import Lib
import Test.QuickCheck

testCreatePixel :: Pixel -> Bool
testCreatePixel Pixel {position=a, color=b} = a == (0,1) && b == (2,3,4)

testDistance :: Float -> Bool
testDistance a = a == 5.0

testDefaultOption :: Option -> Bool
testDefaultOption Option {nbCluster=a, convLim=b, pathToFile=c} =
                a == 0 && b == 0 && c == ""

testCreateCluster :: Cluster -> Bool
testCreateCluster (a, []) = a == (1,2,3)

main :: IO ()
main = do
        quickCheck (testCreatePixel $ dataPixel ["(0,1)", "(2,3,4)"])
        quickCheck (testDistance $ distance (3,4,5) (3,8,2))
        quickCheck (testDefaultOption defaultOption)
        quickCheck (testCreateCluster $ createNewCluster (1,2,3))