import scala.annotation.tailrec

object Polymorphic {
  def main(args: Array[String]): Unit = {
    println(isSorted(Array(1, 2, 3, 4), (a: Int, b: Int) => a < b))
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec def iterate(i: Int): Boolean = 
      if (i >= as.length - 1) true
      else if (!ordered(as(i), as(i + 1))) false
      else iterate(i + 1)
    
    iterate(0);
  }
}