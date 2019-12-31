
module Day20 ( runDay20 ) where

import Data.Attoparsec.Text
import Data.Text (pack)
import Lib (readData')
import System.IO

runDay20 :: [String] -> IO String
runDay20 [] = return "Usage: 20 <filename>"
runDay20 (filename:_) =
    do lines <- withFile filename ReadMode readData'
       return $ show $ Prelude.map ((fmap (`translate` 3)) . parse parseLine . pack) lines
       --return $ show $ translate testCase 3

-- Example of record
-- p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>

testCase = Particle (3,0,0) (2,0,0) (-1,0,0)

data Particle = Particle { position :: (Int, Int, Int)
                         , velocity :: (Int, Int, Int)
                         , acceleration :: (Int, Int, Int)
                         } deriving Show

parseLine :: Parser Particle
parseLine =
    do string (pack "p=")
       pos <- parseTriple
       skipWhile (inClass ", ")
       string (pack "v=")
       vel <- parseTriple
       skipWhile (inClass ", ")
       string (pack "a=")
       acc <- parseTriple
       return $ Particle pos vel acc

parseTriple :: Parser (Int, Int, Int)
parseTriple =
    do char '<'
       x <- signed decimal
       skip (== ',')
       y <- signed decimal
       skip (== ',')
       z <- signed decimal
       char '>'
       return (x, y, z)

translate :: Particle -> Int -> Particle
translate (Particle (x, y, z) (vx, vy, vz) a@(ax, ay, az)) time =
    let evolve co vel acc = co + time*vel + div (acc*time*(1 + time)) 2
        accel vel acc = vel + time*acc
     in Particle (evolve x vx ax, evolve y vy ay, evolve z vz az)
                 (accel vx ax, accel vy ay, accel vz az) a

stopPoint :: Particle -> Maybe Int
stopPoint (Particle co (vx,vy,vz) (ax,ay,az)) =
    let tx = - vx `div` ax
        ty = - vy `div` ay
        tz = - vz `div` az
        max = maximum [tx,ty,tz]
     in if max >= 0
           then Just max
           else Nothing
