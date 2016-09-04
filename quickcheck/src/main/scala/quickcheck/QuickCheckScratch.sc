import scala.math._

import org.scalacheck._
import Prop._
import Gen._
import Arbitrary.arbitrary


//Properties
val propConcatLists = Prop.forAll { (l1: List[Int], l2: List[Int]) => l1.size + l2.size == (l1 ::: l2).size }
propConcatLists.check

val propSqrt = Prop.forAll { (n: Int) => sqrt(n*n) == n }
propSqrt.check

val smallInt = Gen.choose(0, 100)
val propSmallInt = Prop.forAll(smallInt) { n => n >= 0 && n <= 100}
propSmallInt.check

//Conditional Properties
val propMakeList = Prop.forAll { n: Int => (n >= 0 && n < 10) ==> (List.fill(n)("").length == n) }
propMakeList.check

//Labeling Properties
def myMagicFunction(n: Int, m: Int) = n + m;

val complexProp = forAll { (m: Int, n: Int) =>
  val res = myMagicFunction(n, m)
  (res >= m) :| "result > #1" &&
  (res >= n) :| "result > #2" &&
  (res <= m + n) :| "result not sum"
}
complexProp.check

val complexProp2 = forAll { (m: Int, n: Int) =>
  val res = myMagicFunction(n, m)
  ("result > #1" |: res >= m) &&
  ("result > #2" |: res >= n) &&
  ("result not sum" |: res <= m + n)
}
complexProp2.check

val complexProp3 = forAll { (m: Int, n: Int) => (n > 0 && m > 0) ==> {
    val res = myMagicFunction(n, m)
    ("res = " + res) |: all("result > #1" |: res >= m,
                            "result > #2" |: res >= n,
                            "result not sum" |: res <= m + n) //Same as &&
  }
}
complexProp3.check

//Generators
val myGen = for {
  n <- Gen.choose(10, 20)
  m <- Gen.choose(2*n, 500)
} yield (n, m)

val vowel = Gen.oneOf('A', 'E', 'I', 'O', 'U', 'Y')
val vowelFreq = Gen.frequency((3, 'A'), (4, 'E'), (2, 'I'), (3, 'O'), (1, 'U'), (1, 'Y'))

vowel.sample
vowelFreq.sample

//Arbitrary Generators
arbitrary[Int].sample

//Generating case classes
sealed abstract class Tree

case class Node(left: Tree, right: Tree, v: Int) extends Tree
case object Leaf extends Tree

val genLeaf = const(Leaf)
val genNode = for {
  v <- arbitrary[Int]
  left <- genTree
  right <- genTree
} yield Node(left, right, v)

def genTree: Gen[Tree] = oneOf(genLeaf, genNode)
genTree.sample

//Sized Generators
def matrix[T](g: Gen[T]): Gen[Seq[Seq[T]]] = Gen.sized { size =>
  val side = sqrt(size).asInstanceOf[Int]
  Gen.listOfN(side, Gen.listOfN(side, g))
}

//Conditional Generators
val smallEvenInt = Gen.choose(0, 200).suchThat(_ % 2 == 0)
smallEvenInt.sample

//Generating Containers
val genIntList = Gen.containerOf[List, Int](Gen.oneOf(1, 3, 5))
val genStringStream = Gen.containerOfN[Stream, String](3, Gen.alphaStr)
val genBoolArray = Gen.nonEmptyContainerOf[Array, Boolean](true)

genIntList.sample
genStringStream.sample
genBoolArray.sample

//Arbitrary Generator
val evenInt = Arbitrary.arbitrary[Int] suchThat (_ % 2 == 0)
evenInt.sample

val squares = for {
  xs <- Arbitrary.arbitrary[List[Int]]
} yield xs.map(x => x * x)

squares.sample

//Collecting generated Test Data
def ordered(l: List[Int]) = l == l.sorted

val myProp = forAll { l: List[Int] =>
  classify(ordered(l), "ordered") {
    classify(l.length > 5, "large", "small") {
      l.reverse.reverse == l
    }
  }
}
myProp.check

//Gen Heap



