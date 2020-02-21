import scala.annotation.tailrec

object Fibonacci {
  def main(args: Array[String]): Unit = {
    println(fib(5))
  }

  def fib(n: Int): Int = {
    @tailrec def fibonacci(n: Int, prev: Int = 0, next: Int = 1): Int = n match {
      case 0 => prev
      case 1 => next
      case _ => fibonacci(n - 1, next, prev + next)
    } 
    fibonacci(n)
  }
}