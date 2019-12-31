{-# LANGUAGE BangPatterns #-}
module Day17
    ( runDay17
    )
    where

import System.IO
import Data.Sequence as Sq
import Data.Foldable

runDay17 :: [String] -> IO String
runDay17 [] = return "Usage: 17 <step_size>"
runDay17 (step_size:_) =
    let (seq, pos) = generate (read step_size) 2017
        (_, pos2) = generate' (read step_size) 50000000
        (Just result) = seq !? (pos + 1)
        (_, result') = pos2
     --in return $ "Element after 2017 is " ++ show result
     in return $ "Element after 0 is " ++ show result'
     -- in return $
     --    if hd == 0
     --       then let (Just result') = seq !? 1 in "Element after 0 is " ++ show result'
     --       else error "Zero is not first..."

generate :: Int -> Int -> (Seq Int, Int)
generate shift max = foldl' (\(seq, pos) val -> iteration shift seq pos val) (singleton 0, 0) [1..max]

generate' :: Int -> Int -> ((Int,Int),(Int,Int))
generate' shift max =
    foldl' (\(prev@(ppos, pval), !fst) el ->
             let new@(np,nv) = iteration' shift el ppos
              in (new, if np == 1
                          then new
                          else fst
                 )
           ) ((0,0), (0,0)) [1..max]

iteration :: Int -> Seq Int -> Int -> Int -> (Seq Int, Int)
iteration shift seq 0 1 = (seq |> 1, 1)
iteration shift seq pos val =
    let depth = Sq.length seq
        new_pos = ((pos + shift) `rem` depth) + 1
     in (insertAt new_pos val seq, new_pos)

iteration' :: Int -> Int -> Int -> (Int, Int)
iteration' shift depth pos =
    (((pos + shift) `rem` depth) + 1, depth)
