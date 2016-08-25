
val xs = Stream.cons(1, Stream.cons(2, Stream.cons(3, Stream.empty)))

val xs2 = Stream(1, 2, 3)

val xs3 = (1 to 3).toStream

val xs4 = 0 #:: xs

def streamRange(lo: Int, hi: Int): Stream[Int] = {
  if (lo >= hi) Stream.empty
  else lo #:: streamRange(lo + 1, hi)
}

def listRange(lo: Int, hi: Int): List[Int] = {
  if (lo >= hi) Nil
  else lo :: listRange(lo + 1, hi)
}

listRange(1, 10)
streamRange(1, 10)

