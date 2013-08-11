object Assert {
  def that(expr: Boolean) = if (expr) println("pass") else println("fail !!")
  def equal(expected: AnyVal, actual: AnyVal) = {
    if (expected == actual) println("pass") else println("fail - expected: " + expected + ", actual: " + actual)
  }
}

object Main {
  type Set = Int => Boolean
  def contains(s: Set, elem: Int): Boolean = s(elem)
  def empty(): Set = (x: Int) => false
  def single(elem: Int): Set = (x: Int) => x == elem
  def union(s: Set, t: Set): Set = (x: Int) => contains(s, x) || contains(t, x)
  def intersection(s: Set, t: Set): Set = (x: Int) => contains(s, x) && contains(t, x)
  def diff(s: Set, t: Set): Set = (x: Int) => contains(s, x) && ! contains(t, x)
  def set(elems: List[Int]): Set = if (elems.isEmpty) empty() else union(single(elems.head), set(elems.tail))
  def filter(s: Set, p: Int => Boolean): Set = (x: Int) => contains(s, x) && p(x)
  def forall(s: Set, p: Int => Boolean): Boolean = {
     def iter(a: Int): Boolean = {
       if (a > 1000) true
       else if (contains(s, a) && !p(a)) false
       else iter(a + 1)
     }
     iter(-1000)
  }
  def exists(s: Set, p: Int => Boolean): Boolean = ! forall(s, (x: Int) => !p(x))
  def map(s: Set, f: Int => Int): Set = (x: Int) => exists(s, (y: Int) => f(y) == x)
}

println("Running basic tests...")
val s1 = Main.single(1)
Assert.that(Main.contains(s1, 1))
Assert.that(! Main.contains(s1, 2))

val s1234 = Main.set(List(1,2,3,4))
val s3456 = Main.set(List(3,4,5,6))
val union = Main.union(s1234, s3456)
val intersection = Main.intersection(s1234, s3456)
val diff = Main.diff(s1234, s3456)

println("Running union tests...")
Assert.that(Main.contains(union, 1))
Assert.that(Main.contains(union, 3))
Assert.that(Main.contains(union, 5))
Assert.that(! Main.contains(union, 7))

println("Running intersection tests...")
Assert.that(! Main.contains(intersection, 1))
Assert.that(Main.contains(intersection, 3))
Assert.that(! Main.contains(intersection, 5))
Assert.that(! Main.contains(intersection, 7))

println("Running diff tests...")
Assert.that(Main.contains(diff, 1))
Assert.that(! Main.contains(diff, 3))
Assert.that(! Main.contains(diff, 5))
Assert.that(! Main.contains(diff, 7))

println("Running filter tests...")
val filter = Main.filter(s1234, (x: Int) => x % 2 == 0)
Assert.that(! Main.contains(filter, 1))
Assert.that(Main.contains(filter, 2))
Assert.that(! Main.contains(filter, 3))
Assert.that(Main.contains(filter, 4))

val even = (x: Int) => x % 4 == 0
println("Running forall tests...")
Assert.that(Main.forall(even, (x: Int) => x % 2 == 0))
Assert.that(! Main.forall(even, (x: Int) => x % 3 == 0))

println("Running exists tests...")
Assert.that(Main.exists(even, (x: Int) => x >= 100 && x <= 105))
Assert.that(! Main.exists(even, (x: Int) => x % 2 == 1))

println("Running map tests...")
val squares = Main.map((x: Int) => true, (x: Int) => x * x)
Assert.that(Main.contains(squares, 1))
Assert.that(! Main.contains(squares, 2))
Assert.that(Main.contains(squares, 4))
Assert.that(Main.contains(squares, 9))
