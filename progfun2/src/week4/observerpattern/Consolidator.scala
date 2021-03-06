package week4.observerpattern

class Consolidator(observed: List[BankAccount]) extends Subscriber {

  observed.foreach(_.subscribe(this))

  private var total: Int = _
  compute()

  private def compute() = {
    total = observed.map(_.currentBalance).sum
  }

  def handler(pub: Publisher): Unit = compute()

  def totalBalance = total
}
