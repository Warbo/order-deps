module AllTests (tests) where

import           Data.Aeson
import qualified Data.Graph as DG
import qualified Data.Text as T
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

geomList :: Gen a -> Gen [a]
geomList g = do x <- g
                oneof [pure [], (x:) <$> geomList g]

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary ASTId where
  arbitrary = do i <- arbitrary
                 d <- geomList arbitrary
                 return ASTId {
                     aId = i
                   , aDeps = d
                   }

instance Arbitrary a => Arbitrary (DG.SCC a) where
  arbitrary = oneof [DG.AcyclicSCC <$> arbitrary,
                     DG.CyclicSCC  <$> listOf1 arbitrary]

tests :: TestTree
tests = testGroup "Tests" [
  QC.testProperty "Decode and encode are inverse"   prop_parse,
  QC.testProperty "Extracting graph preserves size" prop_extract,
  QC.testProperty "Cyclic dependencies are grouped" prop_combinesCycles,
  QC.testProperty "Independent IDs are grouped"     prop_combinesIndependent,
  QC.testProperty "Leaves are taken from graph"     prop_leavesAreInGraph,
  QC.testProperty "Leaves have no dependencies"     prop_leavesHaveNoDeps,
  QC.testProperty "Can get deps of SCCs"            prop_sccDepsMatch,
  QC.testProperty "At least one SCC has no deps"    prop_alwaysDepLessScc]

prop_parse :: ASTId -> Bool
prop_parse x =  (decode . encode $ x)  == Just x

prop_extract :: ASTId -> Bool
prop_extract a = case extractGraphable a of
                      (_, _, t) -> length t == length (aDeps a)

prop_combinesCycles i1 i2 = i1 /= i2 ==>
  case Grapher.group [ASTId { aId = i1, aDeps = [i2] },
                      ASTId { aId = i2, aDeps = [i1] }] of
       [DG.CyclicSCC as] -> let as' = map atomToId as
                             in as' `elem` [[i1, i2], [i2, i1]]
       _                   -> False

prop_combinesIndependent i1 i2 = i1 /= i2 ==>
  case bigGroup [ASTId { aId = i1, aDeps = [] },
                 ASTId { aId = i2, aDeps = [] }] of
       [DG.CyclicSCC as] -> let as' = map atomToId as
                             in as' `elem` [[i1, i2], [i2, i1]]

prop_leavesAreInGraph ids' = counterexample debug test
  where test    = all (`elem` ids) leafIds
        ids     = fillGraph ids'
        leafIds = leaves ids
        debug   = show (("ids", ids), ("leaves", leafIds))

prop_leavesHaveNoDeps ids' = counterexample debug test
  where test    = all (null . aDeps) leafIds
        ids     = fillGraph ids'
        leafIds = leaves ids
        debug   = show (("ids", ids), ("leaves", leafIds))

prop_sccDepsMatch ids = all (`elem` astDs) sccDs && all (`elem` sccDs) astDs
  where sccDs = concatMap sccDeps (group' (map extractGraphable ids))
        astDs = filter (not . inIds) (concatMap (map idToAtom . aDeps) ids)
        inIds x = any ((== atomToId x) . aId) ids

prop_alwaysDepLessScc a as = not (null (depLess (group' xs)))
  where xs = stripUnknownDeps (a:as)

-- Helpers

fillGraph :: [ASTId] -> [ASTId]
fillGraph [] = []
fillGraph (aid:aids) = aid:aids ++ map simple unknownDeps
  where unknownDeps = filter (`notElem` ids) (aDeps aid)
        ids         = map aId aids
        simple x    = ASTId { aId = x, aDeps = [] }
