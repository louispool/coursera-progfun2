package week4.frp

class BankAccount {

  val balance = Var(0)

  def deposit(amount: Int): Unit = {
    if (amount > 0) {
      val b = balance()
      balance() = b + amount
    }
  }

  def withdraw(amount: Int): Unit = {
    val b = balance()
    if (0 < amount && amount <= b) {
      balance() = b - amount
    } else throw new Error("insufficient funds")
  }


}
