{-# LANGUAGE BangPatterns #-}
module Day15
    ( runDay15
    )
    where

import Data.Word
import Data.Bits
import Data.List (foldl')

runDay15 :: [String] -> IO String
runDay15 _ = return $ show $ duel2 5000000

generator :: Word -> Word -> Word
generator factor prev =
    (prev * factor) `rem` divisor

divisor = 2147483647

generator' :: Word -> Word -> Word -> Word
generator' factor fltr prev =
    let value = generator factor prev
        tail = generator' factor fltr value
     in if value `rem` fltr == 0
           then value
           else tail

generatorA = generator' 16807 4
generatorB = generator' 48271 8

judge :: Word -> (Word, Word) -> Word
judge cntr (genA, genB) =
    if ((genA `xor` genB) .&. 0xFFFF) == 0
       then cntr + 1
       else cntr

duel :: Word -> (Word, Word, Word)
duel num =
    foldl' (\(!le,!cntr,!ri) _ ->
             let (genA, genB) = (generator 16807, generator 48271)
                 (le', ri') = (genA le, genB ri)
                 cntr' = judge cntr (le', ri')
              in (le', cntr', ri')
           ) (634,0,301) [1..num]

duel2 :: Word -> (Word, Word, Word)
duel2 num =
    foldl' (\(!le,!cntr,!ri) _ ->
             let (le', ri') = (generatorA le, generatorB ri)
                 cntr' = judge cntr (le', ri')
              in (le', cntr', ri')
           ) (634,0,301) [1..num]

duel' :: Word -> (Word, Word)
duel' num =
    let (genA, genB) = (generator 16807, generator 48271)
        loop 0 acc = acc
        loop !cntr (!le, !ri) = 
            let p@(le', ri') = (genA le, genB ri)
                -- !cntr' = judge jcntr p
             in loop (cntr - 1) (le', ri')
     in loop num (65,8921)
