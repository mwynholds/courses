object Main {
  def assertEqual(expected: AnyVal, actual: AnyVal) = {
    if (actual == expected)
      println("pass")
    else
      println("fail - expected: " + expected + ", actual: " + actual)
  }

  def pascal(c: Int, r: Int): Int = {
    if (c > r)  return 0
    if (c == 0 || c == r) return 1
    pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  def balance(chars: List[Char]): Boolean = {
    def calc(count: Integer, char: Char): Integer = {
      if (char == '(') return count + 1
      if (char == ')') return count - 1
      count
    }

    def bal(count: Integer, chars: List[Char]): Boolean = {
      if (chars.isEmpty && count == 0) return true
      if (chars.isEmpty && count != 0) return false
      if (count < 0) return false
      bal(calc(count, chars.head), chars.tail)
    }

    bal(0, chars)
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) return 0
    if (money == 0) return 1
    if (money <  0) return 0
    val left = money - coins.head
    //println(s"count($left, $coins) + count($left, ${coins.tail})")
    countChange(left, coins) + countChange(left, coins.tail)
  }
}

println("Running pascal tests...")
Main.assertEqual(3, Main.pascal(1, 3))
Main.assertEqual(10, Main.pascal(3, 5))

println("Running balance tests...")
Main.assertEqual(true, Main.balance("(if (zero? x) max (/ 1 x))".toList))
Main.assertEqual(true, Main.balance("I told him (that it's not (yet) done). (But he wasn't listening)".toList))
Main.assertEqual(false, Main.balance(":-)".toList))
Main.assertEqual(false, Main.balance("())(".toList))

println("Running countChange tests...")
Main.assertEqual(1, Main.countChange(1, List(1)))
Main.assertEqual(3, Main.countChange(4, List(1, 2)))
Main.assertEqual(4, Main.countChange(15, List(1, 5)))
Main.assertEqual(19, Main.countChange(12, List(1, 2, 3)))
