
{-# LANGUAGE FlexibleContexts,MultiWayIf #-}
module Day6
    ( runDay6
    ) where

import Control.Monad.ST
import Data.Array.MArray
import Data.Array.Unboxed
import Data.Array.ST
import Data.STRef
import System.IO
import Data.List as Lst
import Debug.Trace (trace)

--infixl 0 <$
--(<$) :: a -> (a -> b) -> b
--(<$) arg f = f arg
--
type Pool s = STUArray s Int Word
type PoolST s = ST s (Pool s)
type LogPool = UArray Int Word
type Hist s = STRef s [LogPool]

runDay6 :: [String] -> IO String
runDay6 [] = return "Usage: 6 <list of initial values>"
runDay6 values =
    let init = map read values :: [Word]
     in return . ("Result is: " ++) . show . run $ init

searchMaxUArray :: UArray Int Word -> (Int, Word)
searchMaxUArray =
    Lst.foldr
        (\(i,e) (ai, ae) ->
            if e > ae
               then (i, e)
               else (ai, ae)
        )
        (0,0)
    . assocs

--redistU :: UArray Int Word -> (Int,Word) -> UArray Int Word
--redistU arr (i,e) =
--    let arr' = arr // [(i,0)] 
--        (lo,up) = getBounds arr
--        loop a v i' =
--            if | v == 0 -> a
--               | i' > up -> loop a v lo
--               | otherwise -> loop (a // [(i', v)]) (v-1) (i'+1)
--     in loop arr' e (i+1)

searchMaxMArray :: STUArray s Int Word -> ST s (Int, Word)
searchMaxMArray arr =
    getAssocs arr >>= return . Lst.foldl' (\(ai, ae) (i,e) ->
        if e > ae
           then (i, e)
           else (ai, ae)) (0,0) 

redist :: STUArray s Int Word -> (Int,Word) -> ST s ()
redist arr elem@(i,e) =
    do writeArray arr i 0
       bo@(lo, up) <- getBounds arr
       let loop i' e' =
             if | i' > up -> loop lo e'
                | e' == 0 -> return ()
                | otherwise -> do v <- readArray arr i'
                                  writeArray arr i' (v+1)
                                  loop (i'+1) (e'-1)
        in loop (i+1) e

run :: [Word] -> (Word, Word)
run xs = runST $
     do arr <- newListArray (1, length xs) xs
        hist <- newSTRef ([] :: [LogPool])
        let loop c =
                do arrays <- readSTRef hist
                   uarr <- freeze arr
                   let sLoop co (x:xs) = if uarr == x
                                            then (True, co)
                                            else sLoop (co+1) xs
                       sLoop co [] = (False, co)
                       (predic, cyc) = sLoop 1 arrays
                   if predic
                      then return (c, cyc)
                      else searchMaxMArray arr
                           >>= redist arr
                           >> modifySTRef hist ( uarr :)
                           >> loop (c+1)

        loop 0
