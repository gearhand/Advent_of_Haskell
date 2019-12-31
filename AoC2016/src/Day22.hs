{-# LANGUAGE MultiWayIf #-}
module Day22
    ( runDay22
    ) where

import System.IO
import Data.List as Lst
import Data.Ord

runDay22 :: [String] -> IO String
runDay22 [] = return "Usage: 22 <input filename>"
runDay22 (filename:xs) =
    withFile filename
        ReadMode
        processor
--    >>= output

processor :: Handle -> IO String 
    {-processor handle = processor' $ loop handle
    where loop hand = hGetLine hand >>= (\line -> line : loop hand)-}
processor handle = 
    hGetLine handle >>
    hGetLine handle >>
    loop handle [] >>=
       (return . ((++) "Success: ") . show . Lst.length . searchValid1 . sortAvailDesc)
    --first <- hGetLine handle
    --second <- hGetLine handle
    --third <- hGetLine handle
    --putStrLn $ "First line: " ++ first
    --putStrLn $ "Second line: " ++ second
    --putStrLn $ "Third line: " ++ third
    --return "Success!"
    where loop :: Handle -> [Entry] -> IO [Entry]
          loop hndl entries =
            hIsEOF hndl >>=
                (\eof -> if eof
                            then return entries
                            else hGetLine hndl >>= 
                                   (\line -> case parseEntries line of
                                               Just e  -> loop hndl $ e:entries
                                               Nothing -> return entries
                                   )
                )

output :: IO a -> IO String
output x = undefined

--(Filesystem Size Used Avail Use%)
type Entry = (String, Word, Word, Word, Word)
type EntryStr = (String, String, String, String, String)

conversion :: EntryStr -> Maybe Entry
conversion (fs, sz, us, av, cent) =
    case (fs , reads sz, reads us, reads av, reads cent) of
         (fs', [(v1, "T")], [(v2, "T")], [(v3, "T")], [(v4, "%")]) -> Just (fs', v1, v2, v3, v4)
         _ -> Nothing

parseEntries :: String -> Maybe Entry
parseEntries str = case words str of
                        [fs, sz, us, av, cent] -> conversion (fs, sz, us, av, cent)
                        _                      -> Nothing

sortAvailDesc :: [Entry] -> [Entry] -- Beware eta reduction!
sortAvailDesc = Lst.sortOn (Data.Ord.Down . (\(_,_,_,av,_) -> av))

searchValid :: [Entry] -> [(Entry, Entry)]
searchValid (a@(_,_,used,avail,_):es) =
    Lst.foldl'
        (\acc b@(_,_,us,av,_) ->
            if | used <= av  -> (a, b) : acc
               | us <= avail -> (b, a) : acc
               | otherwise   -> acc
        )
        (searchValid es)
        es 

searchValid [] = []

searchValid1 :: [Entry] -> [(Entry, Entry)]
searchValid1 all =
    Lst.foldl' (\acc e -> acc ++ loop e all) [] all 
        where loop :: Entry -> [Entry] -> [(Entry, Entry)]
              -- Предполагается, что els отсортирован по убыванию
              loop a@(_,_,us,_,_) (b@(_,_,_,av',_):els) =
                  if | (us <= av') && a /= b -> (a, b) : loop a els
                     | otherwise             -> []
              loop _ [] = []

