import Day1
import Day2
import Day3

import Data.Either
import Test.Hspec

main = hspec $ do
    describe "day1" $
        it "checks all test cases" $
            testDay1 `shouldBe` Right ()

    describe "day2" $ do
        it "checks first part" $
            testRun2 [ [5,1,9,5]
                     , [7,5,3]
                     , [2,4,6,8]
                     ] `shouldBe` 18

        it "check binaryNod" $
            shouldBe ( testNod 2 5 1
                     >> testNod 4 8 4
                     >> testNod 50 25 25
                     >> testNod 100 25 25
                     ) (Right ())

        it "check nodRow" $
            shouldBe ( testNodRow [5,9,2,8] 4
                     >> testNodRow [9,4,7,3] 3
                     >> testNodRow [3,8,6,5] 2
                     ) (Right ())

    describe "day3" $ do
        it "checks first part" $
            testRun3 `shouldBe` (Right ())


--main = let check = case1 () >>= case2 >>= case3 >>= case4 >>=
--                   case1' >>= case2' >>= case3' >>= case4' >>= case5'
--        in case check of
--                Right _ -> putStrLn "Success!"
--                Left ca -> putStrLn $ "Failed in " ++ ca

case1 _ = if 3 == run "1122" then Right () else Left "case1"
case2 _ = if 4 == run "1111" then Right () else Left "case2"
case3 _ = if 0 == run "1234" then Right () else Left "case3"
case4 _ = if 9 == run "91212129" then Right () else Left "case4"

case1' _ = if 6 == run' "1212" then Right () else Left "case1'"
case2' _ = if 0 == run' "1221" then Right () else Left "case2'"
case3' _ = if 4 == run' "123425" then Right () else Left "case3'"
case4' _ = if 12 == run' "123123" then Right () else Left "case4'"
case5' _ = if 4 == run' "12131415" then Right () else Left "case5'"

testDay1 = case1 () >>= case2 >>= case3 >>= case4 >>=
           case1' >>= case2' >>= case3' >>= case4' >>= case5'
