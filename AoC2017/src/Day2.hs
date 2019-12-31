{-# LANGUAGE MultiWayIf #-}
module Day2
    ( runDay2
    , testRun2
    , testNod
    , testNodRow
    )
    where

import Data.List as Lst
import Data.Char
import System.IO
import Control.Monad
import Debug.Trace(trace)

checksum :: [[Word]] -> Word
checksum xs =
    let inner =
            Lst.foldl' (\acc'@(min,max) el ->
                               if | el < min  -> (el, max)
                                  | el > max  -> (min, el)
                                  | otherwise -> acc'
                       ) (infinum, supremum)
     in Lst.foldl' (\acc xs' ->
                        let (min,max) = inner xs'
                         in acc + max - min
                   ) 0 xs

checkRow :: [Word] -> Word
checkRow xs =
    delta' $ Lst.foldl'
        (\acc el -> checkMin el $ checkMax el acc) (supremum, infinum) xs
   where delta' (min, max) = max - min
         checkMax el acc@(min, max)
             | el > max  = (min, el)
             | otherwise = acc
         checkMin el acc@(min, max)
             | el < min  = (el, max)
             | otherwise = acc

infinum = 0 :: Word
supremum = fromIntegral (-1) :: Word

binaryNOD :: Word -> Word -> Word
binaryNOD m n = case (m, n) of
                     (0,n') -> n'
                     (m',0) -> m'
                     (1,n') -> 1
                     (m',1) -> 1
                     (m',n')-> if | m' == n' -> m'
                                  | even m' && even n' -> 2 * binaryNOD (div m' 2) (div n' 2)
                                  | even m' -> binaryNOD (div m' 2) n'
                                  | even n' -> binaryNOD m' (div n' 2)
                                  | n' > m' -> binaryNOD (div (n' - m') 2) m
                                  | n' < m' -> binaryNOD (div (m' - n') 2) n

nodRow :: [Word] -> Word
nodRow row =
    loop row 0
        where loop (x:xs) acc = loop xs $
                Lst.foldl' (\acc' el -> let nod = binaryNOD x el
                                     in if | nod == x  -> acc' + div el x
                                           | nod == el -> acc' + div x el
                                           | otherwise -> acc'
                           ) acc xs
              loop [] acc = acc

testNod :: Word -> Word -> Word -> Either String ()
testNod m n nod = if binaryNOD m n == nod
                     then Right ()
                     else Left $ "testNod failed at " ++ show (m,n,nod)

testNodRow :: [Word] -> Word -> Either String ()
testNodRow xs res = if nodRow xs == res
                       then Right ()
                       else Left $ "testNodRow failed at " ++ show (xs, res)


-- Mind eta reduction!
testRun2 :: [[Word]] -> Word
testRun2 = Lst.foldl' (\acc el -> acc + checkRow el) 0
    --where traced x = trace ("checkRow: " ++ show x) x

runDay2 :: [String] -> IO String
runDay2 [] = return "Usage: 2 <input filename>"
runDay2 (filename:xs) =
    withFile filename
        ReadMode
        processor
--    >>= output

processor :: Handle -> IO String 
processor handle = loop handle 0
    where loop hndl result = 
            hIsEOF hndl >>=
                (\eof -> if eof
                            then return $ "Result is " ++ show result
                            else hGetLine hndl >>=
                                       (loop hndl
                                       . (+) result
                                       -- . checkRow
                                       . nodRow
                                       . map (\w -> case reads w of
                                                          [(n, "")] -> n :: Word
                                                          _         -> 0
                                              )
                                         
                                       . words
                                       )
                                   
                )
