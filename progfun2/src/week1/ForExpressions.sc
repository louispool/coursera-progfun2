
def isPrime(n: Int) = (2 until n).forall(d => (n % d) != 0)

def f(n: Int) = (1 until n).flatMap(i =>
                  (1 until i).filter(j => isPrime(i + j)).map
                    (j => (i, j)))

def ff(n: Int) = {
  for {
    i <- 1 until n
    j <- 1 until i
    if isPrime(i + j)
  } yield (i, j)
}

f(5)
ff(5)

def ff1(n: Int) = for (x <- 1 until n) yield x*x
def f1(n: Int) = (1 until n).map(x => x*x)

f1(5)
ff1(5)


def ff2(n: Int) = for (x <- 1 until n; if isPrime(x)) yield x*x
def f2(n: Int) = (1 until n).withFilter(x => isPrime(x)).map(x => x*x)

ff2(5)
f2(5)

def ff3(n: Int) = for (x <- 1 until n; y <- 1 until x; if isPrime(x + y)) yield (x, y)
def f3(n: Int) = (1 until n).flatMap(x => (1 until x).withFilter(y => isPrime(x + y)).map(y => (x, y)))

ff3(5)
f3(5)


def mapFun[T, U](xs: List[T], f: T => U): List[U] = for (x <- xs) yield f(x)

def flatMap[T, U](xs: List[T], f: T => Iterable[U]): List[U] = for (x <- xs; y <- f(x)) yield y

def filter[T](xs: List[T], p: T => Boolean): List[T] = for (x <- xs if p(x)) yield x



