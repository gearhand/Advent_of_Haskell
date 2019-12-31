{-# LANGUAGE MultiWayIf #-}
module Day3
    ( runDay3
    , testRun3
    )
    where

import Data.List as Lst
import Data.Sequence as Seq
import Data.Char
import System.IO
import Control.Monad
import Debug.Trace(trace)

closestSquareTop :: Word -> Word
closestSquareTop num =
    let fst = ceiling $ sqrt $ fromIntegral num
     in if odd fst then fst else fst + 1

idxFromCoords :: (Int, Int) -> Word
idxFromCoords (x,y) =
    let lvl = max (abs x) (abs y)
        side = lvl * 2 + 1
        --corner = side*side
        start = (side-2)*(side-2) + 1
     in if lvl == 0
           then 1
           else fromIntegral $
                  if | y == -lvl-> (start+3*(side-1)) + (lvl-1) + x
                     | x == -lvl-> (start+2*(side-1)) + (lvl-1) - y
                     | y == lvl -> (start+side-1)     + (lvl-1) - x
                     | x == lvl ->  start             + (lvl-1) + y

coordsFromIdx :: Word -> (Int, Int)
coordsFromIdx idx' =
    let idx = fromIntegral idx'
        lvl = quot (side-1) 2
        --corner = side*side
        start = (side-2)*(side-2) + 1
        side = fromIntegral $ closestSquareTop idx'
     in if lvl == 0
           then (0,0)
           else if | idx < start+(side-1)   -> (lvl, idx - start - (lvl-1))
                   | idx < start+2*(side-1) -> ((start+side-1) + (lvl-1) - idx, lvl)
                   | idx < start+3*(side-1) -> (-lvl, (start+2*(side-1)) + (lvl-1) - idx)
                   | idx < start+4*(side-1) -> (idx - (start+3*(side-1)) - (lvl-1), -lvl)

surround :: (Int, Int) -> [(Int, Int)]
surround (x, y) = [ (x+1,y  )
                  , (x+1,y+1)
                  , (x+1,y-1)
                  , (x  ,y+1)
                  , (x  ,y-1)
                  , (x-1,y-1)
                  , (x-1,y+1)
                  , (x-1,y  )
                  ]

path (x, y) = abs x + abs y
                     
runDay3 :: [String] -> IO String
runDay3 [] = return "Usage: 3 <input number>"
runDay3 (cell:_) =
    return $
      case parseOneNumber cell of
           Nothing -> "Cannot parse input!"
           Just num ->
             --"Result path is " ++ show (processor num)
             "Result value is " ++ show (processor' num)

processor :: Word -> Word
processor num =
    let side = closestSquareTop num
        lvl = quot (side-1) 2
        --(xmax, ymin) = (lvl, -lvl)
        corner = side * side
        delta = corner - num
        max_path = 2 * lvl
     in if lvl > 0
           then max (delta `rem` max_path) (max_path - (delta `rem` max_path))
           else 0

    {-
processor' :: Word -> Word
processor' num =
    let foo = 10
     in viewr $
        foldr (\idx generated ->
                let count = foldl' (\sum idx' -> sum + (generated !! idx')) 0
                            .Lst.filter (< idx)
                            .map idxFromCoords
                            .surround
                            .coordsFromIdx
                 in if count > num
                       then 
              ) (1,Seq.singleton 1) [2..]-}

processor' :: Word -> Word
processor' num =
    let while acc (x:xs)= let res = nextElem acc x
                           in if res <= num
                                 then while (acc|>res) xs
                                 else res
     in while (Seq.singleton 1) [2..]

nextElem :: Seq Word -> Word -> Word
nextElem seq idx =
            foldl' (\sum idx' -> sum + index seq (idx' - 1) ) 0
            . map fromIntegral
            . Lst.filter (< idx)
            . map idxFromCoords
            . surround
            . coordsFromIdx $ idx


parseOneNumber :: (Read n, Num n) => String -> Maybe n
parseOneNumber str = case reads str of
                          [(num, "")] -> Just num
                          _           -> Nothing


testRun3 :: Either String ()
testRun3 = do
    case1
    case2
    case3
    case4

case1 = if 0 == processor 1 then Right () else Left "case1"
case2 = if 3 == processor 12 then Right () else Left "case2"
case3 = if 2 == processor 23 then Right () else Left "case3"
case4 = if 31 == processor 1024 then Right () else Left "case4"
case5 = if 3 == processor 10
           && 2 == processor 11
           && 3 == processor 12
           && 4 == processor 13
           && 3 == processor 14
           && 2 == processor 15
           && 3 == processor 16
           && 4 == processor 17
           && 3 == processor 18
           && 2 == processor 19
           && 3 == processor 20
           && 4 == processor 21
           && 3 == processor 22
           && 2 == processor 23
           && 3 == processor 24
           && 4 == processor 25
           then Right () else Left "case 5"
