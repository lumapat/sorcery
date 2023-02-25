import qualified Sorcery.GameTest (tests)

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
      [ Sorcery.GameTest.tests
      ]

