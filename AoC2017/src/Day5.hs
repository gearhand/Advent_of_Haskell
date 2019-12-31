{-# LANGUAGE FlexibleContexts #-}
module Day5 (runDay5) where

import Control.Monad.ST
import Data.Array.MArray
import Data.Array.ST
import System.IO
import Data.List as Lst

--type Commands s = STUArray s Word Int
--
--


type Commands s = STUArray s Int Int

interpret :: [Int] -> Int
interpret xs =
    let init :: ST s (STUArray s Int Int)
        init = newListArray (1, length xs) xs
     in runST $ init >>=
          \arr ->
            getBounds arr >>=
                \(lo,up) -> --loop (lo, 0)
                    let --loop :: (Int,Int) -> ST s Int
                        loop (idx, cntr) =
                            if idx < lo || idx > up
                               then return cntr
                               --else return 0
                               else readArray arr idx >>=
                                        \v ->
                                            writeArray arr idx (v+1) >>
                                                loop (idx + v, cntr + 1)
                     in loop (lo,0)
                    --loop arr' (lo, up) (lo, 0)

interpret' :: [Int] -> Int
interpret' xs =
    let init :: ST s (STUArray s Int Int)
        init = newListArray (1, length xs) xs
     in runST $
        do arr <- init
           (lo,up) <- getBounds arr
           let --loop :: (Int,Int) -> ST s Int
               loop (idx, cntr) =
                   if idx < lo || idx > up
                      then return cntr
                      --else return 0
                      else do v <- readArray arr idx
                              writeArray arr idx (mutate v)
                              loop (idx + v, cntr + 1)
               mutate c = if c >= 3
                             then c - 1
                             else c + 1
            in loop (lo,0)
                        

--loop :: Commands s -> (Int,Int) -> (Int,Int) -> ST s Int
--loop arr (lo, up) (idx, cntr) =
--    if idx < lo || idx > up
--       then return cntr
--       else readArray arr idx >>=
--                \v ->
--                    writeArray arr (v+1) >>
--                        loop' (idx + v, cntr + 1)
--    where loop' = loop arr (lo,up)

-- В императивном стиле:
-- нужно в цикле while проверять границы
-- если границы внутри -- выполняем прыжок, инкремент элемента и инкремент счётчика
-- если снаружи границ -- выходим из цикла и возвращаем счётчик

runDay5 [] = return "Usage: 5 <input file>"
runDay5 (file:_) =
    withFile file ReadMode processor

--compute :: [Int] -> Int
--compute xs = runST $ sum' xs
compute xs = runSTUArray $ sum' xs
    where init :: [Int] -> ST s (STUArray s Int Int)
          init xs = newListArray (1, length xs) xs
          sum' xs = init xs >>=
                    \arr ->
                        getBounds arr >>=
                            \(lo,up) ->
                                Lst.foldl' (\arr' idx -> arr' >>= \a -> readArray a idx >>= writeArray a idx.(+1) >> return a) (return arr) [lo..up]
                                 --Lst.foldl' (\acc idx -> readArray arr idx >>= \v -> (+v) <$> acc) (return 0) [lo..up]
                                 --Lst.foldl' (\acc idx -> (+) <$> readArray arr idx <*> acc) (return 0) [lo..up]



processor :: Handle -> IO String
processor handle = readAll handle []
    where readAll hndl list =
              hIsEOF hndl >>=
                  \eof -> if eof
                             then return $ "Result is " ++ (show.interpret'.reverse $ list)
                             else hGetLine handle >>=
                                    (\str ->
                                     let [(v, "")] = reads str
                                      in readAll handle (v:list)
                                    )
