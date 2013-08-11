object Assert {
  def true(expr: Boolean) = {
    if (expr)
      println("pass")
    else
      println("fail")
  }

  def equal(expected: AnyVal, actual: AnyVal) = {
    if (expected == actual)
      println("pass")
    else
      println("fail - expected: " + expected + ", actual: " + actual)
  }
}
