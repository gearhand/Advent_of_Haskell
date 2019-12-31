module Day1
    ( runDay1
    , run
    , run'
    )
    where

import Data.List as Lst
import Data.Char
import System.IO
import Control.Monad

captcha :: [Word] -> Word
captcha (first:other) =
    let (ce, last) = Lst.foldl'
                        (\(acc, prev) el -> (if prev == el then acc + prev else acc , el))
                        (0, first)
                        other
     in if first == last then ce + last else ce

captcha' :: [Word] -> Word
captcha' xs =
    let len = Lst.length xs
        shift = len `div` 2
        compl x = (x + shift) `mod` len
     in fst $ Lst.foldl'
            (\(acc, idx) el -> ( if el == (xs !! compl idx)
                                    then acc + el
                                    else acc
                               , idx+1
                               )
            )
            (0, 0)
            xs

parse :: String -> [Word]
parse = map (fromIntegral.digitToInt)

run :: String -> Word
run = captcha.parse

run' :: String -> Word
run' = captcha'.parse

runDay1 :: [String] -> IO String
runDay1 [] = return "Usage: 1 <input filename>"
runDay1 (filename:xs) =
    withFile filename
        ReadMode
        processor
--    >>= output

processor :: Handle -> IO String 
    {-processor handle = processor' $ loop handle
    where loop hand = hGetLine hand >>= (\line -> line : loop hand)-}
processor handle = show.run' <$> hGetLine handle
