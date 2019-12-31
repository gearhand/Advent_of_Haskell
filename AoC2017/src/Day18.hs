{-# LANGUAGE MultiWayIf, FlexibleInstances #-}
module Day18
    ( runDay18
    )
    where

import System.IO
--import Text.ParserCombinators.ReadP
import Data.Char
import Data.ByteString.Char8
import Data.Attoparsec.ByteString.Char8
import Data.Sequence as Sq
--import Data.Foldable
import Lib (readData')
import Data.Map.Strict as MapS
--import Control.Monad.ST
--import Data.STRef
import Data.Vector as Vec
import Control.Concurrent.STM.TQueue
import Control.Concurrent.STM.TVar
import Control.Concurrent (forkIO)

runDay18 :: [String] -> IO String
runDay18 [] = return "Usage: 18 <filename>"
runDay18 (filename:_) = 
     do strings' <- withFile filename ReadMode readData'
        return . show $ do actions <- traverse parse' strings'
                           let init = State 0 MapS.empty 0 Sq.empty Sq.empty
                           let loop st =
                                  case applyAction (Vec.fromList actions) st of
                                       Left fin -> fin
                                       Right st' -> loop st'
                           return $ loop init
    --show <$> withFile filename ReadMode readData'

type Memory = Map Char Int
data State = State { lastFreq :: Int
                   , memory :: Memory
                   , position :: Int
                   , outbox :: Seq Int
                   , inbox :: Seq Int
                   }
                   deriving Show

data State2 = State2 { def :: Int
                     , memory2 :: Memory
                     , position2 :: Int
                     }

--data State3 = State3 (TVar (Bool, Seq Int))
type MyResult = Either (Maybe Int) State


type SharedState = TVar (Bool, Seq Int)

translate :: String -> IO (Either String [StTrans])
translate filename = traverse parse' <$> withFile filename ReadMode readData'

runInstance :: State2 -> SharedState -> [StTrans] -> IO ()
runInstance init shar acts = undefined
    where vActs = Vec.fromList acts
          pos = position2 init
          action = vActs Vec.!? pos
          state' = init { position2 = pos + 1 }
          -- This is usual state transforming loop : tail recursive!
          -- we need to check other thread in recv command
          -- and set out state in snd
          -- and terminate everything, if out of actions
          loop st =
              case vActs Vec.!? position st of
                   Nothing -> Nothing
                   Just act ->
                       case act st of
                            Left res -> res
                            Right st' -> loop st'


dumbAction =
    do sharState <- newTVarIO (False, Sq.empty)
       Right actions <- translate "filename"
       forkIO $ runInstance (State2 0 MapS.empty 0) sharState actions
       forkIO $ runInstance (State2 1 MapS.empty 0) sharState actions
       return ()



applyAction :: Vector (State -> MyResult) -> State -> MyResult
applyAction actions state =
    let pos = position state
        action = actions Vec.!? position state
        state' = state { position = pos + 1 }
     in case action of
             Nothing -> Left Nothing
             Just act -> act state'

type StTrans = State -> MyResult
instance Show StTrans where
    show _ = "state transformer"

runSet :: Char -> Int -> StTrans
runSet reg val st = Right $ st { memory = insert reg val (memory st) }

runAdd :: Char -> Int -> StTrans
runAdd reg val st = Right $ st { memory = MapS.update (Just . (val +)) reg (memory st) }

runMul :: Char -> Int -> StTrans
runMul reg val st = Right $ st { memory = MapS.update (Just . (val *)) reg (memory st) }

runMod :: Char -> Int -> StTrans
runMod reg val st = Right $ st { memory = MapS.update (Just . (`rem` val)) reg (memory st) }

runJgz :: Int -> Int -> StTrans
runJgz regv val st =
    let --mem = memory st
        --regv = findWithDefault 0 reg mem
        pos = position st
     in if regv == 0
           then Right st
           else if | val == 0 -> Left Nothing
                   | val == 1 -> Right st 
                   | val >  1 -> Right $ st { position = pos + val - 1 }
                   | val <  0 -> Right $ st { position = pos + val - 1 }


runGeneric :: (Int -> StTrans) -> Char -> StTrans
runGeneric f key st = f val st
    where val = findWithDefault 0 key (memory st)


--runSet = runGeneric runSet
--runAdd = runGeneric runAdd
--runMul = runGeneric runMul
--runMod = runGeneric runMod
--runJgz = runGeneric runJgz


--runSnd :: Char -> StTrans
--runSnd reg st = Right $ st { lastFreq = findWithDefault 0 reg (memory st) }
runSnd :: Int -> StTrans
runSnd val st = Right $ st { lastFreq = val }

runSnd' :: Int -> StTrans
runSnd' val st = Right $ st 

--runRcv :: Char -> StTrans
--runRcv reg st =
--    if findWithDefault 0 reg (memory st) == 0
--       then Right st
--       else Left $ Just $ lastFreq st

runRcv :: Int -> StTrans
runRcv regv st =
    if regv == 0
       then Right st
       else Left $ Just $ lastFreq st

parse' :: String -> Either String (State -> MyResult)
parse' =
    let pSnd = parseSingle "snd" runSnd
        pRcv = parseSingle "rcv" runRcv
        pSet = parseDouble "set" runSet
        pAdd = parseDouble "add" runAdd
        pMul = parseDouble "mul" runMul
        pMod = parseDouble "mod" runMod
        pJgz = parseDouble' "jgz" runJgz
        choice' = choice [pSnd, pRcv, pSet, pAdd, pMul, pMod, pJgz] :: Parser (State -> MyResult)
     in parseOnly choice' . pack

    {-
parseSingle :: (Char -> StTrans) -> Parser StTrans
parseSingle con =
    do skipSpace
       reg <- satisfy isLower
       return $ con reg

parseDouble :: Parser Char
parseDouble =
    do skipSpace
       reg <- satisfy isLower
       skipSpace
       return $ reg
       -}

parseSingle :: String -> (Int -> StTrans) -> Parser StTrans
parseSingle name f =
    do string (pack name)
       skipSpace
       ei <- eitherP (satisfy isLower) (signed decimal)
       return $
           case ei of
                Left ch -> runGeneric f ch
                Right n -> f n
       
parseDouble :: String -> (Char -> Int -> StTrans) -> Parser StTrans
parseDouble name f =
    do string (pack name)
       skipSpace
       reg <- satisfy isLower
       skipSpace
       ei <- eitherP (satisfy isLower) (signed decimal)
       return $
           case ei of
                Left ch -> runGeneric (f reg) ch
                Right n -> f reg n

parseDouble' :: String -> (Int -> Int -> StTrans) -> Parser StTrans
parseDouble' name f =
    do string (pack name)
       skipSpace
       ei_reg <- eitherP (satisfy isLower) (signed decimal)
       skipSpace
       ei <- eitherP (satisfy isLower) (signed decimal)
       return $
           \st ->
               let mem = memory st in
            case (ei_reg, ei) of
                 (Left reg, Left ch) -> let regv = findWithDefault 0 reg mem
                                         in runGeneric (f regv) ch st
                 (Left reg, Right n) -> let regv = findWithDefault 0 reg mem 
                                         in f regv n st
                 (Right val, Left ch) -> runGeneric (f val) ch st
                 (Right val, Right n) -> f val n st

