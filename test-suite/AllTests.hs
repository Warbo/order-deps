module AllTests (tests) where

import           Data.Aeson
import           Data.Text
import           Grapher
import           Jparse
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC




-- | Use a sized generator to generate a list of values whose combined size
-- matches the given number.
-- divideBetween :: (Int -> Gen a) -> Int -> Gen [a]
-- divideBetween f 0 = return []
-- divideBetween f n = do size <- choose (1, abs n)
--                        head <- f size
--                        tail <- divideBetween f (n - size)
--                        return (head : tail)
-- you can use that to generate a list of recursive calls


instance Arbitrary Text where
  arbitrary = do
    t <- fmap pack arbitrary
    return t

instance Arbitrary ASTId where
  arbitrary = do
                r <- oneof [pure Nothing, Just <$> (resize 4 arbitrary)]
                ASTId <$> arbitrary <*> arbitrary <*> arbitrary <*> pure r



tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [QC.testProperty "parsed" prop_parse_correct]

prop_parse_correct :: ASTId -> Bool
prop_parse_correct x =  (decode . encode $ x)  == Just x
