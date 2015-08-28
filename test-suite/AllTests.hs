module AllTests (tests) where

import           Data.Aeson
import           Data.Text hiding (length)
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

-- data ASTId =
--   ASTId {  name         :: Text
--          , modu         :: Text
--          , package      :: Text
--          , dependencies :: Maybe [ASTId]
--         } deriving (Show, Generic, Eq)

geomList :: Gen a -> Gen [a]
geomList g = oneof [return <$> g, (:) <$> g <*> geomList g]

instance Arbitrary Text where
  arbitrary = do
    t <- fmap pack arbitrary
    return t

instance Arbitrary ASTId where
  arbitrary = do
                r <- oneof [pure Nothing, Just <$> geomList arbitrary]
                ASTId <$> arbitrary <*> arbitrary <*> arbitrary <*> pure r


tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [QC.testProperty "parsed" prop_parse_correct,
                                     QC.testProperty "extracted" prop_extract_correct]

prop_parse_correct :: ASTId -> Bool
prop_parse_correct x =  (decode . encode $ x)  == Just x

prop_extract_correct :: ASTId -> Bool
prop_extract_correct a@(ASTId _ _ _ Nothing )  = let  (_, _ ,t) = extractGraphable a
                                                 in t == []
prop_extract_correct a@(ASTId _ _ _ (Just x) ) = let  (_, _ ,t) = extractGraphable a
                                                in (length x) == length t



