object Currying {
  def main(args: Array[String]): Unit = {}

  // 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = 
    (a: A) => (b: B) => f(a, b)
  // 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)
}