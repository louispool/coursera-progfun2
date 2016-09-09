import scala

//Implicit conversions

implicit def stringWrapper(s: String) =
  new Seq[Char] {
    def length = s.length
    def apply(i: Int) = s.charAt(i)
  }