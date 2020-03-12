module Main where

import           Control.Monad.Writer.Lazy
import qualified Data.Serializer           as S
import qualified Data.Vector.Unboxed       as U
import           MD5.Lib
import           Test.Hspec
import           Test.Hspec.QuickCheck

main :: IO ()
main = hspec $ do
  describe "MD5" $ do
    context "bitLength" $ do
      it "should give 0 for an empty vector" $
        (bitLength $ U.fromList []) `shouldBe` 0
      it "should give 8 for a vector with 8 items" $
        (bitLength $ U.fromList [1, 2, 3, 4, 5, 6, 7, 8]) `shouldBe` 64
    context "keepLower64Bits" $ do
      it "should convert to a vector of 8 elements" $
        U.length (keepLower64Bits 12345) `shouldBe` 8
      it "should convert to vector and back" $
        U.foldl (+) 0 (U.imap shift (keepLower64Bits 12345)) `shouldBe` 12345
    context "padTo448" $ do
      it "should pad from under 448" $ do
        (bitLength $ padTo448 $ U.singleton 9) `shouldBe` 448
        padTo448 (U.singleton 9) `shouldSatisfy` \x -> (U.toList x == ([9, 128] ++ take 54 (repeat 0)))
      it "should pad from over 488" $ do
        bitLength (padTo448 ((U.singleton 1) <> (U.replicate 56 0))) `shouldBe` 960
        padTo448 ((U.singleton 1) <> (U.replicate 56 0)) `shouldSatisfy` \x -> (U.toList x == ([1] ++ take 56 (repeat 0) ++ [128] ++ take 62 (repeat 0)))

----- helpers
shift 0 a = fromIntegral a
shift i a = 256 * i * fromIntegral a
