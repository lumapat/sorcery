import qualified Sorcery.Data.GameTest (tests)

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
      [ Sorcery.Data.GameTest.tests
      ]
