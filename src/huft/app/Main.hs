module Main where
import System.Environment ( getArgs, getProgName )   
import Data.List
import Data.Text
import Data.String
import System.IO

import Languages as L -- cabal will take care of it
import System.IO (stdin, openFile)
import qualified UFT
import qualified Error as E

translationOf :: String -> Handle -> Handle -> IO (E.Error (IO ()))
translationOf spec =
    case split (=='-') (fromString spec) of 
        [from, to] -> case (L.find (unpack from), L.find (unpack to)) of
                        (Just f, Just t) -> UFT.translate f t
                        _                -> error "oops"
        _          -> error "oops"

-- reportandExit :: Maybe (L.Language, L.Language) -> IO ()
reportandExit Nothing = putStrLn "invalid translation specification"
reportandExit _       = putStrLn "Valid translation specification"

main :: IO ()
main = do  
    progName <- getProgName
    args <- getArgs
    case args of -- stopgap implementation
        spec:infile:_ -> do
            file <- openFile infile ReadMode
            translation <- translationOf spec file stdout
            case E.getError translation of
                Left e -> putStrLn e
                Right r -> r
        spec:_ -> do
            translation <- translationOf spec stdin stdout
            case E.getError translation of
                Left e -> putStrLn e
                Right r -> r
        _ -> putStrLn ("Usage: " ++ progName ++ " inLang-outLang infile/stdin")