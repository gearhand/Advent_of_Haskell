module Day11
    ( runDay11
    ) where

--import Control.Monad.ST
import Control.Applicative
--import qualified Data.Map.Strict as Map
import Data.List
import Data.Bits (xor)
import Data.Word8
import Text.Read
import Text.Printf
import System.IO
import Debug.Trace (trace)
import Data.Maybe (fromMaybe)

runDay11 :: [String] -> IO String
runDay11 [] = return "Usage: 11 <filename>"
runDay11 (filename:_) =
    withFile filename ReadMode processor

processor :: Handle -> IO String
processor handle =
    do str <- hGetLine handle
       return $ parse' str

parse' :: String -> String
parse' =
    ("Result is:" ++)
    . show
    . maximum
    -- . pathLen
    -- . normalForm
    . map path2
    . foldl' (\lst@(x:_) s -> ((addVec3 . fromString) s x) : lst) [(Vec3 (0,0,0))]
    . splitComma

splitComma :: String -> [String]
splitComma s = last:res
    where (last, res) = foldr (\el (wrd, wrds) -> case el of
                                                       ',' -> ([], wrd:wrds)
                                                       _   -> (el:wrd, wrds)
                              ) ([], []) s

convert :: [String] -> [Int]
convert nums = fromMaybe (error "Cannot parse input!") (mapM readMaybe nums)

data Move = North
          | South
          | NorthEast
          | NorthWest
          | SouthEast
          | SouthWest
          deriving Show

fromString :: String -> Move
fromString str =
    case str of
         "n"  -> North
         "s"  -> South
         "ne" -> NorthEast
         "nw" -> NorthWest
         "se" -> SouthEast
         "sw" -> SouthWest
         _    -> error $ "Cannot parse direction: " ++ str

newtype Vector f = Vec (f, f) deriving Show
newtype Vector3 f = Vec3 (f, f, f) deriving Show

addVec3 :: Floating a => Move -> Vector3 a -> Vector3 a
addVec3 mv vec@(Vec3 (n, e, w)) =
    case mv of
         North -> Vec3 (n + 1, e, w)
         South -> Vec3 (n - 1, e, w)
         NorthEast -> Vec3 (n, e, w - 1)
         SouthWest -> Vec3 (n, e, w + 1)
         SouthEast -> Vec3 (n, e + 1, w)
         NorthWest -> Vec3 (n, e - 1, w)

normalW :: Floating a => Vector3 a -> Vector3 a
normalW (Vec3 (n, e, w)) = Vec3 (n - w, e - w, 0)

normalE :: Floating a => Vector3 a -> Vector3 a
normalE (Vec3 (n, e, w)) = Vec3 (n - e, 0, w - e)

normalN :: Floating a => Vector3 a -> Vector3 a
normalN (Vec3 (n, e, w)) = Vec3 (0, e - n, w - n)

trail :: Num a => Vector3 a -> a
trail (Vec3 (n, e, w)) = abs n + abs e + abs w
--trail (Vec3 (n, e, w)) = sum $ map abs [n,e,w]

path2 :: (Floating a, Ord a) => Vector3 a -> a
path2 vec =
    minimum $ map trail $ [normalW, normalE, normalN] <*> [vec]

pathLen :: (RealFrac a, Floating a) => Vector3 a -> Int
pathLen vec = ceiling . sqrt $ vecScalar vec vec

vecScalar :: Floating a => Vector3 a -> Vector3 a -> a
vecScalar (Vec3 (nl, el, _)) (Vec3 (nr, er, _)) =
    nl*nr + el*er - 0.5*nl*er - 0.5*el*nr

thrd :: Floating a => a
thrd = pi/6

addVec :: (Floating a) => Move -> Vector a -> Vector a
addVec mv vec@(Vec (x,y)) =
    case mv of
         North -> Vec (x, y+1)
         South -> Vec (x, y-1)
         NorthEast -> Vec (x + cos thrd, y + sin thrd)
         NorthWest -> Vec (x - cos thrd, y + sin thrd)
         SouthEast -> Vec (x + cos thrd, y - sin thrd)
         SouthWest -> Vec (x - cos thrd, y - sin thrd)
