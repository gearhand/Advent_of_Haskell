{-# LANGUAGE FlexibleContexts #-}
module Day14
    ( runDay14
    ) where

import Day10
import Lib
import Data.Bits (testBit)
import Text.Printf
import Data.List as Lst
--import Data.Matrix as Mtx
import qualified Data.Set as Se
import Data.Array.IArray as IArr
import Control.Monad.ST
import Data.STRef

runDay14 :: [String] -> IO String
runDay14 [] = return "Usage: 14 <key_string>"
runDay14 (keyString:_) =
    let (used, repr) = process' keyString
        rgns = process2 keyString
     in return $ show rgns

newtype Sector = Sector Bool

instance Show Sector where
    show (Sector True)  = "#"
    show (Sector False) = "."

process' :: String -> (Int, String)
process' input =
    foldl'( \(ai,as) rowN ->
             let seed = input ++ "-" ++ show rowN
                 hash = condensed seed
                 (inUse, repr) = processRow hash
              in (ai + inUse, as ++ '\n' : show repr)
          ) (0, "") [0..127]

process2 :: String -> Int
process2 = length . regions . graph

processRow :: [Int] -> (Int,[Sector])
processRow =
    foldl' (\(ai, as) int ->
             let (nai, nas) = procOctet int
              in (ai + nai, as ++ nas)
           ) (0, [])

procOctet :: Int -> (Int,[Sector])
procOctet hex =
    foldl' (\(cnt, repr) idx ->
             if testBit hex idx
                then (cnt + 1, Sector True  : repr)
                else (cnt    , Sector False : repr)
           ) (0,[]) ([0..7] :: [Int])

rowToSet :: Int -> [Sector] -> Covered
rowToSet line nodes =
    let (_, result) =
            foldl' (\(idx, set) (Sector used) ->
                     if used
                        then (idx+1, Se.insert (idx,line) set )
                        else (idx+1, set)
                   ) (0, Se.empty) nodes
     in result

testSeed = "flqrgnkx"

graph :: String -> Covered
graph input =
    foldl'( \set rowN ->
             let seed = input ++ "-" ++ show rowN
                 hash = condensed seed
                 (inUse, repr) = processRow hash
                 covRow = rowToSet rowN repr
              in Se.union set covRow
          ) Se.empty [0..127]

regions :: Covered -> [Uncovered]
regions gr =
    let (uncov, cov) = getRegion gr
     in if Se.null gr
           then []
           else uncov : regions cov

type Uncovered = Se.Set (Int,Int)
type Covered = Se.Set (Int,Int)

toSet :: [Sector] -> Covered
toSet lst =
    snd $
    foldl' (\(cnt, set) (Sector node) ->
             let coord = cnt `quotRem` 128
              in if node then (cnt + 1, Se.insert coord set)
                         else (cnt + 1, set)
           ) (0, Se.empty) lst

-- foo :: Covered -> ST s [Uncovered]
-- foo matrix =
--     let uncovM = newSTRef (Se.empty :: Uncovered)
--         covM = newSTRef matrix
--      in do coRef <- covM
--            unRef <- uncovM
--            node <- (head . Se.toList) <$> (readSTRef coRef)
--            writeSTRef unRef (Se.singleton node)
--            -- cycle start
--            coRef `modifySTRef` (Se.delete node) 
--            nei <- (neighbours node) <$> readSTRef coRef
--            if Se.null nei
--               then undefined
--               else do modifySTRef unRef (Se.union nei)
--                       modifySTRef coRef (Se.\\ nei)
--                       cov <- readSTRef coRef
--                       map (\f -> f cov) $ neighbours <$> (Se.toList nei)
--

getRegion :: Covered -> (Uncovered, Covered)
getRegion graph =
    let initNode = head $ Se.toList graph
        initNei = neighbours graph initNode 
        loop uncov cov  =
            let nei = Se.foldl' (\acc el -> Se.union acc (neighbours graph el)) Se.empty cov
                uncov' = Se.union uncov cov
                cov' = Se.difference nei uncov'
             in if Se.null cov'
                   then uncov'
                   else loop uncov' cov'
        result = loop (Se.singleton initNode) initNei
     in (result, Se.difference graph result)
                       


-- walk :: (IArray a Sector) => a (Int,Int) Sector -> (Int, Int) -> Uncovered
-- walk arr start@(i,j) =
--     let (Sector sect) = arr ! start
--         init = Se.empty
--      in if sect
--            then let next = neighbours arr start
--                     init' = Se.insert (i,j) init
--                     next' = filter id $ map (arr !) next
--            else init

neighbours :: Covered -> (Int,Int) -> Covered
neighbours cov (i,j) =
    let adj = Se.fromList [(i+1,j), (i-1,j), (i, j+1), (i, j-1)]
     in Se.intersection cov adj

neighbours' :: (Int,Int) -> STRef s Covered -> ST s Covered
neighbours' (i,j) ref =
    let adj = Se.fromList [(i+1,j), (i-1,j), (i, j+1), (i, j-1)]
     in readSTRef ref >>=
             (\cov -> modifySTRef ref (`Se.difference` adj) >> return (Se.intersection cov adj))


