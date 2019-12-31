module Main where

import Day22
import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= parseArgs >>= putStrLn

parseArgs :: [String] -> IO String
parseArgs [] = return "Usage: ./app <day number>"
parseArgs (x:xs) = 
             case x of
                  "22" -> runDay22 xs
                  _    -> return $ "No implementation for day " ++ x
