module Day9 
    ( runDay9
    ) where

--import Control.Monad.ST
import Control.Applicative
--import qualified Data.Map.Strict as Map
import Data.List
import Data.Word8
import qualified Data.ByteString.Char8 as BS
import Data.Attoparsec.ByteString.Char8
--import Text.Read
import System.IO
import Debug.Trace (trace)

runDay9 :: [String] -> IO String
runDay9 [] = return "Usage: 9 <filename>"
runDay9 (filename:_) =
    withFile filename ReadMode processor

processor :: Handle -> IO String
processor handle =
    do bString <- BS.hGetContents handle
       return $ parse' bString

    {- Для данной задачи актуальна следующая грамматика:
        garbage = '<' (canceled | [^>])* '>'
        canceled = '!' ANY
        group = '{' (group|garbage)* '}'
     -}

parse' :: BS.ByteString -> String
parse' bstring =
    let result = parse (parseGroup 1) bstring
     in case result of
             Done _ r -> show r
             Partial _ -> error "Parsing not finished!"
             Fail {} ->
                    error $ show result

parseCancel :: Parser Char
parseCancel = char '!' >> anyChar

parseGroup :: Int -> Parser (Int, Int)
parseGroup score =
    do char '{'
       firstG <- option (0,0) $ parseGroup (score + 1) <|> ((,) 0 <$> parseGarbage)
       groups <- many' $ char ',' >> (parseGroup (score + 1) <|> ((,) 0 <$> parseGarbage))
       char '}'
       return $ foldl' (\(as,ag) (es,eg) -> (as+es, ag+eg)) (score, 0) (firstG:groups)

parseGarbage :: Parser Int
--parseGarbage = undefined
parseGarbage =
    do char '<'
       garb <- many' ((parseCancel >> return 0) <|> (notChar '>' >> return 1))
       char '>'
       return $ sum garb

toW8 :: Char -> Word8
toW8 = toEnum . fromEnum

toChar :: Word8 -> Char
toChar = toEnum . fromEnum

special = toW8 <$> ['{', '}', '<', '>', '!']
