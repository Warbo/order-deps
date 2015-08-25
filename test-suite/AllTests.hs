module AllTests (tests) where

import           Data.Text
import           Test.Tasty
import           Test.Tasty.QuickCheck as QC


-- type Atom = (Text, Text, Text)
-- type Graphable = [(Atom, Atom,[Atom])]
-- ASTId




tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [QC.testProperty "dummy" quickCheckProps]

quickCheckProps :: Int -> Bool
quickCheckProps x = x + 1 == 1 + x
