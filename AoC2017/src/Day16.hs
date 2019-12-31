module Day16
    ( runDay16
    )
    where


import Text.Parsec
import Text.Parsec.Char
import Data.Word
import Data.Array
import Data.List (find, foldl')
import System.IO
import Lib
import Control.Monad.ST
import qualified Data.Vector.Unboxed as Vec

data Command = Spin Int
             | Exchange Int Int
             | Partner Char Char
             deriving Show

type VecRegistry = Vec.Vector Char

runDay16 :: [String] -> IO String
runDay16 [] = return "Usage: 16 <filename>"
runDay16 (filename:_) =
    withFile filename ReadMode (processor2 1000000000)

processor :: Handle -> IO String
processor handle =
    do (input:_) <- readData handle
       let (Right cmds) = parse commands "" input
       return $ Vec.toList $ foldl' (flip applyCommand) realInput cmds

processor2 :: Int -> Handle -> IO String
processor2 repeats handle =
    do (input:_) <- readData handle
       let (Right cmds) = parse commands "" input
       let transformation = foldl' (flip (.)) id (applyCommand <$> cmds)
       let p = period transformation realInput
       let repeats' = repeats `rem` p
       return $ Vec.toList $ foldl' (\acc _ -> transformation acc) realInput [1..repeats']

period :: (VecRegistry -> VecRegistry) -> VecRegistry -> Int
period transf regist = 
       let loop x i = 
               if realInput == transf x
                  then i
                  else loop (transf x) (i+1)
        in loop regist 1


-- initTest :: Array Word Char
-- initTest = listArray (0,4) "abcde"

initTest :: VecRegistry
initTest = Vec.fromList "abcde"

-- realInput :: Registry
-- realInput = listArray (0,15) "abcdefghijklmnop"
realInput :: VecRegistry
realInput = Vec.fromList "abcdefghijklmnop"

-- applyCommand :: Command -> Registry -> Registry
applyCommand :: Command -> VecRegistry -> VecRegistry
applyCommand (Spin x) input =
    let point = Vec.length input -  x
        (fst, sec) = Vec.splitAt point input
     in sec Vec.++ fst

applyCommand (Exchange x y) input =
    let fst' = input Vec.! x
        snd' = input Vec.! y
     in input Vec.// [(y, fst'), (x, snd')]

-- applyCommand (Partner n1 n2) input =
--     let pairs = assocs input
--         (Just (fst', _)) = find (\(_,v) -> v == n1) pairs
--         (Just (snd', _)) = find (\(_,v) -> v == n2) pairs
--      in input // [(fst',n2), (snd', n1)]

applyCommand (Partner n1 n2) input =
    let (Just fst') = Vec.findIndex (== n1) input
        (Just snd') = Vec.findIndex (== n2) input
     in input Vec.// [(fst',n2), (snd', n1)]

process :: [Command] -> VecRegistry -> VecRegistry
process cmds input =
    foldl' (flip applyCommand) input cmds

-- (s[0-15]|x[0-15]/[0-15]|p[a-p]/[a-p])
commands :: Parsec String u [Command]
commands = command `sepBy1` char ','

command :: Parsec String u Command
command = choice [pSpin, pExchange, pPartner]

pSpin :: Parsec String u Command
pSpin = Spin . read <$> (char 's' >> many1 digit)

pExchange :: Parsec String u Command
pExchange = do char 'x'
               d1 <- read <$> many1 digit
               char '/'
               d2 <- read <$> many1 digit
               return $ Exchange d1 d2
-- pExchange = char 'x' >> many1 digit >>= \d1 -> char '/' >> many1 digit >>= \d2 -> return $ Exchange (read d1) (read d2)

pPartner :: Parsec String u Command
pPartner = do char 'p'
              d1 <- oneOf "abcdefghijklmnop"
              char '/'
              d2 <- oneOf "abcdefghijklmnop"
              return $ Partner d1 d2

testString = "s1,x3/4,pe/b"
