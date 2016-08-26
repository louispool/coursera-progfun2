
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

def from(n: Int): Stream[Int] = n #:: from(n+1)

//Stream of all natural numbers
val nats = from(0)

//Stream of all even numbers
val evens = nats.map(_ * 2)

evens.take(50).toList


//Sieve of Eratosthenes (Prime number calculation)
def sieve(s: Stream[Int]): Stream[Int] = {
  s.head #:: sieve(s.tail.filter(_ %s.head != 0))
}

val primes = sieve(from(2))

primes.take(100).toList


//Decoupling our isGoodEnough test that terminates convergence for calculation of square roots
def sqrtStream(x: Double): Stream[Double] = {
  def improve(guess: Double) = (guess + x / guess) / 2
  lazy val guesses: Stream[Double] = 1 #:: guesses.map(improve)
  guesses
}

sqrtStream(4).take(10).toList

def isGoodEnough(guess: Double, x: Double) = math.abs((guess*guess - x)/x) < 0.000000001

sqrtStream(4).filter(isGoodEnough(_, 4)).take(1).toList








