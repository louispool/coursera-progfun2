package week1

/*
from https://en.wikipedia.org/wiki/JSON

{
  "firstName": "John",
  "lastName": "Smith",
  "address": {
    "streetAddress": "21 2nd Street",
    "city": "New York",
    "state": "NY",
    "postalCode": "10021-3100"
  },
  "phoneNumbers": [
    {
      "type": "home",
      "number": "212 555-1234"
    },
    {
      "type": "office",
      "number": "646 555-4567"
    },
    {
      "type": "mobile",
      "number": "123 456-7890"
    }
  ],
  "children": [],
  "spouse": null
}

*/

object JSONObj {

  abstract class JSON
  case class JSeq(elems: List[JSON]) extends JSON
  case class JObj(bindings: Map[String, JSON]) extends JSON
  case class JNum(num: Double) extends JSON
  case class JStr(str: String) extends JSON
  case class JBool(b: Boolean) extends JSON
  case object JNull extends JSON

  val data = JObj(Map("firstName" -> JStr("John"),
                      "lastName" -> JStr("Smith"),
                      "address" -> JObj(Map("streetAddress" -> JStr("21 2nd Street"),
                                            "city" -> JStr("New York"),
                                            "state" -> JStr("NY"),
                                            "postalCode" -> JStr("10021-3100"))),
                      "phoneNumbers" -> JSeq(List(JObj(Map("type" -> JStr("home"),
                                                           "number" -> JStr("212 555-1234"))),
                                                  JObj(Map("type" -> JStr("office"),
                                                           "number" -> JStr("646 555-4567"))),
                                                  JObj(Map("type" -> JStr("mobile"),
                                                           "number" -> JStr("123 456-7890"))))),
                      "children" -> JSeq(Nil),
                      "spouse" -> JNull))

   def show(json: JSON): String = json match {
     case JSeq(elems) => "[" + elems.map(show).mkString(", ") + "]"
     case JObj(bindings) => {
       val assocs = bindings.map({ case (key, value) => "\"" + key + "\": " + show(value) })
       "{" + assocs.mkString(", ") + "}"
     }
     case JNum(num) => num.toString
     case JStr(str) => "\"" + str + "\""
     case JBool(b) => b.toString
     case JNull => "null"
   }

  def main(args: Array[String]) {
    println(show(data))
  }
}
