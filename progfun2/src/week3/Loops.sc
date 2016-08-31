
def power (x: Double, exp: Int): Double = {
  var r = 1.0
  var i = exp
  while (i > 0) { r = r*x; i = i-1 }
  r
}

power(2, 2)


def WHILE(condition: => Boolean)(command: => Unit): Unit = {
  if (condition) {
    command
    WHILE(condition)(command)
  }
  else ()
}

def powerWHILE(x: Double, exp: Int): Double = {
  var r = 1.0
  var i = exp
  WHILE (i > 0) {
    r = r * x; i = i - 1
  }
  r
}

powerWHILE(2, 2)

def REPEAT(command: => Unit)(condition: => Boolean): Unit = {
  command
  if (condition) ()
  else REPEAT(command)(condition)
}

//For LOOP
for (i <- 1 until 3; j <- "abc") { println(i + " " + j) }

//equivalent to
(1 until 3).foreach(i => "abc".foreach(j => println(i + " " + j)))





