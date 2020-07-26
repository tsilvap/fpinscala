object GettingStarted {
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(a: Int, b: Int, k: Int): Int =
      if (k == n) a else go(b, a+b, k+1)
    go(0, 1, 0)
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(as: Array[A]): Boolean =
      if (as.length <= 1) true
      else if (!ordered(as(0), as(1))) false
      else go(as.tail)

    go(as)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  def compose[A,B,C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
