module AllTests (tests) where

import           Data.Aeson
import           Data.Text hiding (length)
import           Grapher
import           HS2AST.Tests.Generators
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC
import Types

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
geomList g = do x <- g
                oneof [pure [], (x:) <$> geomList g]

instance Arbitrary Text where
  arbitrary = pack <$> arbitrary

instance Arbitrary ASTId where
  arbitrary = do i <- arbitrary
                 d <- geomList arbitrary
                 return ASTId {
                     aId = i
                   , aDeps = d
                   }

tests :: TestTree
tests = testGroup "Tests" [QC.testProperty "parsed"    prop_parse,
                           QC.testProperty "extracted" prop_extract]

prop_parse :: ASTId -> Bool
prop_parse x =  (decode . encode $ x)  == Just x

prop_extract :: ASTId -> Bool
prop_extract a = case extractGraphable a of
                      (_, _, t) -> length t == length (aDeps a)
