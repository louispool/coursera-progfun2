package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    i <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(i, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    (a != b) ==> {
      val h = insert(a, insert(b, empty))
      if (a < b)
        findMin(h) == a
      else
        findMin(h) == b
    }
  }

  property("del1") = forAll { a: Int =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h)
  }

  property("del2") = forAll { (a: Int, b: Int) =>
    val h = deleteMin(deleteMin(insert(b, insert(a, empty))))
    isEmpty(h)
  }

  property("sameseq") = forAll { (n: Int) =>
    val h1 = (1 until 10).foldLeft(empty)((h, _) => insert(n, h))
    val h2 = (1 until 10).foldLeft(h1)((h, _) => deleteMin(h))

    findMin(h1) == n && isEmpty(h2)
  }

  def seqOf(h: H, xs: List[Int]): List[Int] = {
    if (isEmpty(h)) xs
    else {
      val ys = findMin(h) :: xs
      seqOf(deleteMin(h), ys)
    }
  }

  property("sortedseq") = forAll { (h: H) =>
    val l = seqOf(h, Nil).reverse
    l == l.sorted
  }

  property("meldmin") = forAll { (h1: H, h2: H) =>
    val h3 = meld(h1, h2)

    val minH3 = findMin(h3)
    val minH2 = findMin(h2)
    val minH1 = findMin(h1)

    minH3 == math.min(minH2, minH1)
  }

  property("meldmin2") = forAll { (h1: H, h2: H, h3: H) =>
    val h4 = meld(h1, h2)
    val h5 = meld(h3, h4)

    val minH5 = findMin(h5)
    val minH4 = findMin(h4)
    val minH3 = findMin(h3)
    val minH2 = findMin(h2)
    val minH1 = findMin(h1)

    minH5 == math.min(minH4, math.min(minH3, math.min(minH2, minH1)))
  }

  property("meldmin3") = forAll { (h1: H, h2: H) =>
    val h3 = meld(h1, h2)
    val h4 = meld(insert(findMin(h1), h2), deleteMin(h1))

    seqOf(h3, Nil) == seqOf(h4, Nil)
  }

  property("meldempty") = forAll { (h1: H) =>
    val h2 = meld(h1, empty)

    findMin(h2) == findMin(h1)
  }
}

