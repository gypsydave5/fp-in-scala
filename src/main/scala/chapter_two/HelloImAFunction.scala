package chapter_two

object HelloImAFunction extends ((Int, Int) => Int) {
  def apply(a: Int, b: Int): Int = a + b
}
