module Day10
    ( runDay10
    , condensed
    ) where

--import Control.Monad.ST
import Control.Applicative
--import qualified Data.Map.Strict as Map
import Data.List
import Data.Bits (xor)
import Data.Word8
import Text.Read
import Text.Printf
import System.IO
import Debug.Trace (trace)
import Data.Maybe (fromMaybe)

runDay10 :: [String] -> IO String
runDay10 [] = return "Usage: 10 <filename>"
runDay10 (filename:_) =
    withFile filename ReadMode processor

condensed :: String -> [Int]
condensed = condense . rounds 64 [0..255] . (++ modifier) . map fromEnum

processor :: Handle -> IO String
processor handle =
    do str <- hGetLine handle
       return $ parse2' str

    {- Для данной задачи актуальна следующая грамматика:
        garbage = '<' (canceled | [^>])* '>'
        canceled = '!' ANY
        group = '{' (group|garbage)* '}'
     -}

parse' :: String -> String
parse' =
    ("Result is:" ++)
    . show
    . result
    . process
    . convert
    . splitComma

parse2' :: String -> String
parse2' =
    ("Result is: " ++)
    . process2
    . (++ modifier)
    . map fromEnum

splitComma :: String -> [String]
splitComma s = last:res
    where (last, res) = foldr (\el (wrd, wrds) -> case el of
                                                       ',' -> ([], wrd:wrds)
                                                       _   -> (el:wrd, wrds)
                              ) ([], []) s

convert :: [String] -> [Int]
convert nums = fromMaybe (error "Cannot parse input!") (mapM readMaybe nums)

process' :: Int -> Int -> [Int] -> (Int, [Int], Int)
process' skip pos lengths =
    foldl' (\(skp,ls, offset) len  ->
             let (hd, tl) = splitAt len ls
                 (hd2, tl2) = splitAt skp (tl ++ reverse hd)
              in (skp+1,tl2 ++ hd2, offset + skp + len)
           ) (skip, init, 0) lengths
    where list = [0..255]
          (x,xs) = splitAt pos list
          init = xs ++ x

--
type SkPaLi = (Int, Int, [Int])
-- The algorithm is: take a list, get it's head with size of len
-- reverse it, append to tail, then take head with size of skip and
-- append it to tail as it is; skip size is increased
process1' :: [Int] -> [Int] -> Int
process1' list lengths =
    let (skip, path, result) =
            foldl' (\(skp, offset, ls) len  ->
                     let (hd, tl) = splitAt len ls
                         (hd2, tl2) = splitAt skp (tl ++ reverse hd)
                      in (skp+1, offset + skp + len, tl2 ++ hd2)
                   ) (0, 0, list) lengths
        size = length result
        step = path `rem` size
        result' = take size $ drop (size - step) $ cycle result
     in product $ take 2 result'

process1 = process1' [0..255]

process = process' 0 0

process2' :: [Int] -> (Int, Int, [Int]) -> (Int, Int, [Int])
process2' lengths (skip, path, list) =
    let (skip', path', result) =
            foldl' (\(skp, offset, ls) len  ->
                     let (hd, tl) = splitAt len ls
                         (hd2, tl2) = splitAt skp (tl ++ reverse hd)
                      in ((skp+1) `rem` size, offset + skp + len, tl2 ++ hd2)
                   ) (skip, path, list) lengths
        size = length list
     in (skip', path' `rem` size, result)

process2 = printResult . condense . rounds 64 [0..255]

trace' x = trace (show x) x

rounds :: Int -> [Int] -> [Int] -> [Int]
rounds num list lengths =
    let (_, path, result) =
            foldl' (\(skip, path', list') _ ->
                     process2' lengths (skip, path', list')
                   ) (0,0,list) [1..num]
        size = length list
        (head', tail') = splitAt (size - path) result
     in tail' ++ head'

condense :: [Int] -> [Int]
condense [] = []
condense hashList =
    let (p1, t1) = splitAt 16 hashList
        block = foldl' xor 0 p1
     in block : condense t1

printResult hashList =
    foldl' (\acc el -> acc ++ printf "%02x" el) "" hashList

modifier :: [Int]
modifier = [17, 31, 73, 47, 23]

testSeq :: [Int]
testSeq = [3,4,1,5]

textRef = [63,144,180,149,1,255,167,84,125,65,188,0,2,254,229,24]

result :: (Int, [Int], Int) -> Int
result (_, list, sum) =
    let len = length list
        offset = len - (sum `mod` len)
     in (list !! offset) * (list !! (offset + 1))

