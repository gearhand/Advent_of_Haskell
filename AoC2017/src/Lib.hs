module Lib
    ( someFunc
    , readData
    , readData'
    ) where

import System.IO
import Control.Monad(liftM)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- tail recursive
readData' :: Handle -> IO [String]
readData' handle =
    loop []
    where loop :: [String] -> IO [String]
          loop xs =
            do eof <- hIsEOF handle
               if eof
                  then return $ reverse xs
                  else hGetLine handle >>=
                        \line -> loop (line:xs)

-- Lazy one-by-one
readData :: Handle -> IO [String]
readData handle =
    loop []
    where loop :: [String] -> IO [String]
          loop xs =
            do eof <- hIsEOF handle
               if eof
                  then return xs
                  else do line <- hGetLine handle
                          tail <- loop xs
                          return (line:tail)
