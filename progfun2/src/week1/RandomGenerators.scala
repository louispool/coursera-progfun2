package week1

object RandomGenerators {

  trait Generator[+T] {
    self => //alias for this

    def generate: T

    def map[S](f: T => S): Generator[S] = new Generator[S] {
      def generate = f(self.generate)
    }

    def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
      def generate = f(self.generate).generate
    }
  }

  //Random ints
  val integers = new Generator[Int] {
    val rand = new java.util.Random
    def generate = rand.nextInt()
  }

  //Random bools
  val booleans = for (x <- integers) yield x > 0

  val booleans2 = integers.map(x => x > 0)

  val booleans3 = new Generator[Boolean] {
    def generate = integers.generate > 0
  }

  //Random pairs
  val pairs = new Generator[(Int, Int)] {
    def generate = (integers.generate, integers.generate)
  }

  def pairs[T, U](t: Generator[T], u: Generator[U]) = for {
    x <- t
    y <- u
  } yield (x, y)

  def pairs2[T, U](t: Generator[T], u: Generator[U]) = t.flatMap(x => u.map(y => (x, y)))

  def pairs3[T, U](t: Generator[T], u: Generator[U]) = new Generator[(T, U)] {
    def generate = (t.generate, u.generate)
  }

  val intPairs = pairs(integers, integers)

  def single[T](x: T): Generator[T] = new Generator[T] {
    def generate = x
  }

  def choose(lo: Int, hi: Int): Generator[Int] =
    for (x <- integers) yield lo + x % (hi - lo)

  //e.g. oneOf("red", "blue", "green")
  def oneOf[T](xs: T*): Generator[T] =
    for (idx <- choose(0, xs.length)) yield xs(idx)

  //Random List
  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyLists else nonEmptyLists
  } yield list

  def emptyLists = single(Nil)

  def nonEmptyLists = for {
    head <- integers
    tail <- lists
  } yield head :: tail


  //Random Tree
  trait Tree

  case class Inner(left: Tree, right: Tree) extends Tree

  case class Leaf(x: Int) extends Tree

  def leafs: Generator[Leaf] = for (x <- integers) yield Leaf(x)

  def inners: Generator[Inner] = for{
    left <- trees
    right <- trees
  } yield Inner(left, right)

  def trees: Generator[Tree] = for {
    isLeaf <- booleans
    tree <- if (isLeaf) leafs else inners
  } yield tree

  def test[T](g: Generator[T], numTimes:Int = 100)
             (test: T => Boolean): Unit = {
    for (i <- 0 until numTimes) {
      val value = g.generate
      assert(test(value), "test failed for " + value)
    }
    println("passed " + numTimes + " tests")
  }

  def main(args: Array[String]) {
    println(trees.generate)

    test(pairs(lists, lists)) {
      case (xs, ys) => (xs ++ ys).length > xs.length
    }

  }
}
