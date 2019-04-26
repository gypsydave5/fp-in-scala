package chapter_one

class ModularCafe {
  def buyCoffee(cc: MCreditCard, p: Payments): MCoffee = {
    val cup = new MCoffee()
    p.charge(cc, cup.price)
    cup
  }
}

class MCoffee {
  def price = 5
}

class Payments {
  def charge(cc: MCreditCard, price: Int): Unit = {}
}

class MCreditCard

