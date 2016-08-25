package week1

object BookDB {

  case class Book(title: String, authors: List[String])

  val books: Set[Book] = Set(Book(title = "Structure and Interpretation of Computer Programs",
                                    authors = List("Abelson, Harald ", "Sussman, Gerald J.")),
                             Book(title = "Introduction to Functional Programming",
                                    authors = List("Bird, Richard ", "Wadler, Phil")),
                             Book(title = "Effective Java",
                                    authors = List("Bloch, Joshua")),
                             Book(title = "Effective Java 2",
                                      authors = List("Bloch, Joshua")),
                             Book(title = "Java Puzzlers",
                                    authors = List("Bloch, Joshua", "Gafter, Neal") ),
                              Book(title = "Programming in Scala",
                                    authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill")))

  def findAuthor(author: String) = for (b <- books; a <- b.authors if a.startsWith(author)) yield b.title
  def findTitle(title: String) = for (b <- books; if b.title.indexOf(title) >= 0) yield b.title

  def findAuthor2(author: String) = {
    books.flatMap(b => b.authors.filter(a => a.startsWith(author)).map(a => b.title))
  }

  def findMultiAuthors = {
    for {
      b1 <- books
      b2 <- books
      if b1 != b2
      a1 <- b1.authors
      a2 <- b2.authors
      if a1 == a2
    } yield a1
  }

  def main(args: Array[String]) {
    println(findTitle("Program"))
    println(findMultiAuthors)
    println(findAuthor("Bird"))
    println(findAuthor2("Bird"))
  }

}

