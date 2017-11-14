import cofeeshop.Cafe

def square(x: Int) = x * x

square(2)

val listOfNums: List[Int] = Range(1, 10).toList

// Referentially Transparent
val x = "Hello, World"
x.reverse
"Hello, World".reverse

//Not
val y = new StringBuilder("Hello")
val z = y.append(", World")
val r1 = y.toString
val r2 = y.toString

//but
val p = new StringBuilder("Hello")
val r3 = p.append(", World").toString
val r4 = p.append(", World").toString

