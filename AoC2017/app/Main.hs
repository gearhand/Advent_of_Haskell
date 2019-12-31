module Main where

import Lib
import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17
import Day18
import Day20

import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= parseArgs >>= putStrLn

parseArgs :: [String] -> IO String
parseArgs [] = return "Usage: ./app <day number> [day arguments]"
parseArgs (x:xs) = 
             case x of
                  "1" -> runDay1 xs
                  "2" -> runDay2 xs
                  "3" -> runDay3 xs
                  "4" -> runDay4 xs
                  "5" -> runDay5 xs
                  "6" -> runDay6 xs
                  "8" -> runDay8 xs
                  "9" -> runDay9 xs
                  "10" -> runDay10 xs
                  "11" -> runDay11 xs
                  "12" -> runDay12 xs
                  "13" -> runDay13 xs
                  "14" -> runDay14 xs
                  "15" -> runDay15 xs
                  "16" -> runDay16 xs
                  "17" -> runDay17 xs
                  "18" -> runDay18 xs
                  "20" -> runDay20 xs
                  _   -> return $ "No implementation for day " ++ x
