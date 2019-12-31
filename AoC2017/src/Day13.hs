module Day13
    ( runDay13
    ) where

--import Control.Applicative
import Text.Parsec
import Text.Parsec.Char
import Data.List as Lst
--import Data.Sequence as Seq
--import Text.Read
--import Text.Printf
import System.IO
import Lib
--import Data.IntMap.Strict as IntMap
--import Data.Foldable
import Debug.Trace (trace)
import Data.Maybe(fromMaybe)

runDay13 :: [String] -> IO String
runDay13 [] = return "Usage: 13 <filename>"
runDay13 (filename:_) =
    withFile filename ReadMode processor

processor :: Handle -> IO String
processor handle =
    readData handle >>=
        return . show . (fmap (loop 0) . (sequence . fmap (parse layer "")))
    where loop n xs =
              let res@(time, depth, hits) = {-trace' $-} conversion' n xs
               in if hits == 0
                     then (n, res)
                     else loop (n+1) xs


periodic :: Int -> Int -> Int
periodic 1 _ = error "Record with period 1!"
periodic 2 timepoint = if even timepoint then 0 else 1
periodic half_period timepoint =
    let p = half_period - 1
        (q,r) = quotRem timepoint p
     in if even q
           then r
           else p - r

freeLayer _ = 0

checker :: Int -> Int -> Int -> Int
checker 0 _ _ = 0
checker range depth time = if periodic range time == 0
                              then (depth+1) * range
                              else 0
-- checker range depth time = depth * periodic range time

-- test = checker <$> [3,2,0,0,4,0,4] <*> [0..6]
test = zipWith checker [3,2,0,0,4,0,4] [0..6]
test' = sum $ zipWith ($) test [0..6]
-- test'' = foldr (\e acc -> acc + test e) 0 [0..6]

conversion :: [(Int,Int)] -> [Int]
conversion recs =
    let (fin,_) = Lst.maximumBy (\(de1,_) (de2,_) -> compare de1 de2) recs
     in Day13.iterate recs [0..fin]

iterate :: [(Int,Int)] -> [Int] -> [Int]
iterate [] [] = []
iterate recs@((depth,range):rs) (time:timer) =
    if depth == time
       then checker range depth time : Day13.iterate rs timer
       else checker 0 0 0 : Day13.iterate recs timer

trace' x = trace (show x) x

conversion' :: Int -> [(Int,Int)] -> (Int, Int, Int)
conversion' start recs =
    foldl' (\(time,dep,hits) e@(de,ra) ->
            let time' = time + de - dep
             in ({-trace'-} time', {-trace'-} de, {-trace' $-} hits + checker ra de time')
          ) (start,0,0) $ {-trace'-} recs



layer :: Parsec String u (Int, Int)
layer = do depth <- (read <$> many1 digit) <* string ": "
           range <- read <$> many1 digit
           return (depth, range)

