import Test.HUnit
import FP4

test1 = TestCase(assertEqual "test1" "a" (compress "a"))
test2 = TestCase(assertEqual "test2" "abcade" (compress "aaaabccaadeeee"))
test3 = TestCase(assertEqual "test3" ([1,2,1]) (compress [1,1,1,2,2,1,1]) )
test4 = TestCase(assertEqual "test4" "1234" (compress "1234") )
test5 = TestCase(assertEqual "test5" (["str","a"]) (compress ["str","str","a"]) )
test6 = TestCase(assertEqual "test6" ([1.0]) (compress [1.0,1.0,1.0]) )

tests = TestList [
    TestLabel "test1" test1,
    TestLabel "test2" test2,
    TestLabel "test3" test3,
    TestLabel "test4" test4,
    TestLabel "test5" test5
  ]