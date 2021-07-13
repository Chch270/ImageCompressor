import System.Environment
import Data.Maybe
import System.Exit
import Lib
import Text.Read

takeArgs :: Option -> [String] -> Maybe Option
takeArgs opts [] = Just opts
takeArgs _ [_] = Nothing
takeArgs opts ("-n":xs:xsy) = readMaybe xs >>= \s -> 
    takeArgs (opts {nbCluster=s}) xsy
takeArgs opts ("-l":xs:xsy) = (readMaybe xs :: Maybe Float) >>= \s -> 
    takeArgs (opts {convLim=s}) xsy
takeArgs opts ("-f":xs:xsy) = takeArgs (opts {pathToFile=xs}) xsy
takeArgs _ (x:xs:xsy) = Nothing

nbrArguments :: [String] -> Int -> Int
nbrArguments (xa:xs) a = nbrArguments xs (a+1)
nbrArguments _ a = a

errorNbrArgs :: Int -> IO ()
errorNbrArgs 6 = return ()
errorNbrArgs _ = exitWith(ExitFailure 84)

main :: IO ()
main =
    getArgs >>= \argvs ->
    case takeArgs defaultOption argvs of
        Nothing -> exitWith (ExitFailure 84)
        Just opts -> initValue opts