module Day12
    ( runDay12
    ) where

import Control.Applicative
import Text.Parsec
import Text.Parsec.Char
import Data.List as Lst
import Data.Sequence as Seq
import Text.Read
import Text.Printf
import System.IO
import Data.IntMap.Strict as IntMap
import Data.Foldable
import Debug.Trace (trace)
import Data.Maybe(fromMaybe)

runDay12 :: [String] -> IO String
runDay12 [] = return "Usage: 11 <filename>"
runDay12 (filename:_) =
    withFile filename ReadMode processor

processor :: Handle -> IO String
processor handle =
    let init = parse' <$> readData handle :: IO (IntMap [Int])
        loop count mp =
            let (_,mp') = cluster mp
             in if IntMap.null mp'
                   then count
                   else loop (count + 1) mp'
     in show . loop 1 . parse' <$> readData handle

    {-readData :: Handle -> IO (Seq Pipe)
readData handle =
    loop Seq.empty
    where loop :: Seq Pipe -> IO (Seq Pipe)
          loop seqs =
            do eof <- hIsEOF handle
               if eof
                  then return seqs
                  else hGetLine handle >>=
                        \line ->
                            case parse pipe "" line of
                                 Left errmsg -> error $ show errmsg
                                 Right p     -> loop (seqs |> p)-}

readData' :: Handle -> IO [String]
readData' handle =
    loop []
    where loop :: [String] -> IO [String]
          loop xs =
            do eof <- hIsEOF handle
               if eof
                  then return $ Lst.reverse xs
                  else hGetLine handle >>=
                        \line -> loop (line:xs)

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

-- cluster :: Seq Pipe -> IO Integer
-- cluster (Pipe id ids:<|seq) =
--     let closed = Data.Set.singleton id
--         opened = Data.List.filter (\item -> member item closed) ids
--         new_set = Data.List.foldr (\item set -> set `Data.Set.insert` item) closed
--         foo = map (Seq.index seq) opened

cluster :: IntMap [Int] -> (Int, IntMap [Int])
cluster mp =
    let (_,init) = fromMaybe (0,[0]) $ IntMap.lookupMin mp
        extract mp' xs = Lst.foldl' (\acc e -> case IntMap.lookup e mp' of
                                               Nothing -> acc
                                               Just el -> el ++ acc
                                    ) [] xs
        cleanup mp' xs = Lst.foldr IntMap.delete mp' xs
        loop steps nodes open =
            let steps' = steps + Lst.length open'
                nodes' = cleanup nodes open
                open'  = Lst.filter (`IntMap.member` nodes') $ extract nodes open
             in if Lst.null open'
                   then (steps', nodes')
                   else loop ({-trace (show steps' ++ " " ++ show open')-} steps') nodes' open'
     in loop 1 mp init

parse' :: [String] -> IntMap [Int]
parse' = Lst.foldr ( \line acc ->
                            case parse pipe "" line of
                                 Left errmsg -> error $ show errmsg
                                 Right (Pipe idx children) -> IntMap.insert idx children acc
                         ) IntMap.empty

    {- Grammar is: node_idx "<->" idx (, idx)* -}

data Pipe = Pipe Int [Int] deriving (Show)

pipe :: Parsec String u Pipe
-- pipe = (many1 digit <* spaces) <> (string "<->" <* spaces) <> many1 digit <> (concat <$> ((many1 digit) `sepBy1` string ", "))
pipe = (node <* (spaces >> string "<->" >> spaces)) >>= (\n -> Pipe n <$> neighbours)

node :: Parsec String u Int
node = read <$> many1 digit

neighbours :: Parsec String u [Int]
neighbours = node `sepBy1` string ", "
