{-# LANGUAGE MultiWayIf #-}
module Day4
    ( runDay4
    --, testRun4
    )
    where

import Data.List as Lst
import Data.Map.Strict as Map
--import Data.Sequence as Seq
import Data.Char
import System.IO
--import Control.Monad
--import Debug.Trace(trace)

runDay4 :: [String] -> IO String
runDay4 [] = return "Usage: 4 <input file>"
runDay4 (file:_) =
    withFile file ReadMode processor

parseOneNumber :: (Read n, Num n) => String -> Maybe n
parseOneNumber str = case reads str of
                          [(num, "")] -> Just num
                          _           -> Nothing


processor :: Handle -> IO String
processor handle = loop handle 0
    where loop hndl start =
            hIsEOF hndl >>=
            \eof -> if eof
                       then return $ "Result is " ++ show start
                       else hGetLine hndl >>=
                            ( \line ->
                                if checkLine' . words $ line
                                   then loop hndl (start+1)
                                   else loop hndl start
                            )

checkLine :: [String] -> Bool
checkLine = while []
    where while acc (x:xs) =
            notElem x acc && while (x:acc) xs
          while _ [] = True

checkLine' :: [String] -> Bool
checkLine' = while []
    where while acc (x:xs) =
            let spec = spectre x
             in spec `notElem` acc && while (spec:acc) xs
          while _ [] = True

spectre :: String -> Map Char Word
spectre =
    Lst.foldl' (\map ch ->
                 insertWith (+) ch 1 map
               ) Map.empty
