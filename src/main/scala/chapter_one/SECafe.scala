package chapter_one

class SECafe {
  def buyCoffee(cc: SECreditCard): SECoffee = {
  val cup = new SECoffee()

  cc.charge(cup.price)

  cup
  }
}

class SECoffee {
  def price: Int = 2
}

class SECreditCard {
  def charge(price: Int) = {}
}
