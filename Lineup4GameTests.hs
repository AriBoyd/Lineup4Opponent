-- import Test.QuickCheck
import LineUp4Game

data TestCase test expected = TestCase (test -> expected) test expected

type TestSuite test expected = [TestCase test expected]

testEquals :: Eq e => TestCase t e -> Bool
testEquals (TestCase function test expected) = function test == expected

evaluateTests :: Eq e => TestSuite t e -> [Bool]
evaluateTests = map testEquals

testscheckWinnerLine :: TestSuite [Space] (Maybe Player)
testscheckWinnerLine = [TestCase checkWinnerLine []]