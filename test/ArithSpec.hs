module ArithSpec where

import           Arith                 (Term, eval, evalBigStep, isVal,
                                        printTerm, termEof)
import           Test.Hspec            (Spec, describe, it, shouldBe,
                                        shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (arbitrary), Gen)
import           Text.Megaparsec       (runParser)

spec :: Spec
spec = do
    describe "parsing" $ do
        prop "round-trip parse and print" $
            \t -> runParser termEof "" (printTerm t) `shouldBe` Right t
    describe "evaluation implementations" $ do
        prop "eval produces a value" $
            \t -> eval t `shouldSatisfy` isVal
        prop "evalBigStep produces a value" $
            \t -> evalBigStep t `shouldSatisfy` isVal
        prop "evalBigStep and eval give the same results" $
            \t -> evalBigStep t `shouldBe` eval t
