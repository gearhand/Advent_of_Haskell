{-# LANGUAGE GADTs, TypeFamilies #-}
module Day8 
    ( runDay8
    ) where

--import Control.Monad.ST
import qualified Data.Map.Strict as Map
import Data.List
import Text.Read
import System.IO
import Debug.Trace (trace)

runDay8 :: [String] -> IO String
runDay8 [] = return "Usage: 8 <filename>"
runDay8 (filename:_) =
    withFile filename ReadMode processor

processor :: Handle -> IO String
processor handle =
    loop 0 Map.empty
        where loop max map =
                do eof <- hIsEOF handle
                   if eof
                      then return . ("Result: " ++) . show $ max -- . result $ map
                      else do line <- hGetLine handle
                              case parse line map of
                                   Right map' -> let max' = result map'
                                                  in if max' > max
                                                        then loop max' map'
                                                        else loop max map'
                                   Left err -> error err

    {- The grammar looks like this:
        record = register operation condition
        register = LOW_ALPHA
        operation = ("inc" | "dec") value
        value = SIGNED_INT
        condition = "if" register comp value
        comp = "<" | ">" | "<=" | ">=" | "==" | "!="
     -}
parse :: String -> Regs -> Either String Regs
parse line map =
    parseRegister (words line) >>= parseOperation >>= parseCondition >>=
        \(Instruct key mod cond) ->
            return $
                let (val', map') =
                        case map Map.!? key of
                             Just val -> (val, map)
                             Nothing -> (0, Map.insert key 0 map)
                 in if cond map'
                       then Map.insert key (mod val') map'
                       else map'

lowAlpha = "abcdefghijklmnopqrstuvwxyz"

checkRegister :: String -> Either String String
checkRegister word =
    let check = foldr (\ch flag -> flag && ch `elem` lowAlpha) True word
     in if check
           then Right word
           else Left "Forbidden symbols in register name"

parseRegister :: [String] -> Either String ([String], Curr ICons)
parseRegister (wrd:wrds) =
    checkRegister wrd >>= Right . Instruct >>= Right . (,) wrds

parseOperation :: ([String], Curr ICons) -> Either String ([String], Curr (Curr ICons))
parseOperation (op:val:wrds, st) =
    let ops = [("inc", (+)), ("dec", (-))]
        value = readEither val :: Either String Int
     in value >>= (\va -> case op `lookup` ops of
                               Just fu -> Right . (,) wrds . st $ (`fu` va)
                               Nothing -> Left "Wrong operation")

checkComparison :: Ord a => String -> Either String (a -> a -> Bool)
checkComparison word
    | word == "<" = Right (<)
    | word == ">" = Right (>)
    | word == "<="= Right (<=)
    | word == ">="= Right (>=)
    | word == "=="= Right (==)
    | word == "!="= Right (/=)

parseCondition :: ([String], Curr (Curr ICons)) -> Either String Instruction
parseCondition (if_:reg:comp:value:_, st) =
    if if_ == "if"
       then do regval <- checkRegister reg
               compval <- checkComparison comp
               value' <- readEither value
               Right . st $ (\map -> case regval `Map.lookup` map of
                                          Just val -> val `compval` value'
                                          Nothing  -> 0 `compval` value'
                            )
       else Left "No \"if\" in record"

data Instruction = Instruct String (Int -> Int) (Regs -> Bool)
type ICons = String -> (Int -> Int) -> (Regs -> Bool) -> Instruction
type family Curr f where
    Curr (a -> b) = b

type Regs = Map.Map String Int

result :: Map.Map String Int -> Int
result map =
    let (r, v) = maximumBy (\(k1,v1) (k2,v2) -> v1 `compare` v2) . Map.toList $ map
     in v
