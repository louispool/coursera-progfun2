import week4.frp.{BankAccount, Signal}

def consolidated(accts: List[BankAccount]): Signal[Int] =
  Signal(accts.map(_.balance()).sum)

val a = new BankAccount()
val b = new BankAccount()
val c = consolidated(List(a, b))

c()
a deposit 20
c()
b deposit 10
c()
