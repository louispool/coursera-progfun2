val f: String => String = { case "ping" => "pong"}

f("ping")
//f("abc")

//Partial Functions
val pf: PartialFunction[String, String] = { case "ping" => "pong" }

pf.isDefinedAt("ping")
pf.isDefinedAt("abc")

val pf2: PartialFunction[List[Int], String] = {
  case Nil => "one"
  case x::y::rest => "two"
}

pf2.isDefinedAt(List(1, 2, 3))
pf2(List(1, 2, 3))

val pf3: PartialFunction[List[Int], String] = {
  case Nil => "one"
  case x :: rest => rest match {
    case Nil => "two"
  }
}

pf3.isDefinedAt(List(1, 2, 3))
pf3(List(1, 2, 3))



