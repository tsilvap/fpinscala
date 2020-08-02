package fpinscala.datastructures


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 3.1
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  // Exercise 3.2
  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  // Exercise 3.3
  def setHead[A](h: A, as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  // Exercise 3.4
  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def loop(l: List[A], n: Int): List[A] = {
      if (n == 0) l
      else loop(l match {
        case Nil => Nil
        case Cons(_, t) => t
      }, n-1)
    }

    loop(l, n)
  }

  // Exercise 3.5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @annotation.tailrec
    def loop(l: List[A], f: A => Boolean): List[A] =
      l match {
        case Nil => Nil
        case Cons(h, t) => {
          if (!f(h)) Cons(h, t)
          else loop(t, f)
        }
      }

    loop(l, f)
  }

  // Exercise 3.6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  /*
   * Official, efficient solution using mutable internal buffers.
   */
  def init2[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]
    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => Nil
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }
    go(l)
  }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // Exercise 3.7
  // We can try to short circuit as follows:
  def product2(ns: List[Double]): Double =
    foldRight(ns, 1.0)((a: Double, b: Double) =>
      b match {
        case 0.0 => 0.0
        case _ => a * b
      })

  // Exercise 3.8
  // Passing Cons and Nil to foldRight constructs an identical list.

  // Exercise 3.9
  def length[A](as: List[A]): Int = foldRight(as, 0)((_, n) => n + 1)

  // Exercise 3.10
  @annotation.tailrec
  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  // Exercise 3.11
  def sumLeft(ns: List[Int]): Int =
    foldLeft(ns, 0)(_ + _)
  def productLeft(ns: List[Double]): Double =
    foldLeft(ns, 1.0)(_ * _)
  def lengthLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((n, _) => n + 1)

  // Exercise 3.12
  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))

  // Exercise 3.13
  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  // Exercise 3.14
  def append2[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)(Cons(_, _))

  // Exercise 3.15
  def flatten[A](ll: List[List[A]]): List[A] =
    foldRight(ll, Nil: List[A])(append2)

  // Exercise 3.16
  def add1(ns: List[Int]): List[Int] =
    foldRight(ns, Nil: List[Int])((n, acc) => Cons(n+1, acc))

  // Exercise 3.17
  def doubleToString(l: List[Double]): List[String] =
    foldRight(l, Nil: List[String])((d, acc) => Cons(d.toString, acc))

  // Exercise 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as: List[A], Nil: List[B])((a, acc) => Cons(f(a), acc))

  // Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as: List[A], Nil: List[A])(
      (a, acc) => if (f(a)) Cons(a, acc) else acc
    )

  // Exercise 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    flatten(map(as)(f))

  // Exercise 3.21
  def filter2[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else Nil)

  // Exercise 3.22
  def add(as: List[Int], bs: List[Int]): List[Int] =
    as match {
      case Nil => Nil
      case Cons(ha, ta) => bs match {
        case Nil => Nil
        case Cons(hb, tb) => Cons(ha+hb, add(ta,tb))
      }
    }

  // Exercise 3.23
  def zipWith[A,B,C](as: List[A], bs: List[B])(f: (A,B) => C): List[C] =
    as match {
      case Nil => Nil
      case Cons(ha, ta) => bs match {
        case Nil => Nil
        case Cons(hb, tb) => Cons(f(ha,hb), zipWith(ta,tb)(f))
      }
    }

  // Exercise 3.24
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    def startsWith[A](sup: List[A], sub: List[A]): Boolean =
      sup match {
        case Nil => sub == Nil
        case Cons(hsup, tsup) => sub match {
          case Nil => true
          case Cons(hsub, tsub) => (hsup == hsub) && startsWith(tsup, tsub)
        }
      }

    sub match {
      case Nil => true
      case Cons(hsub, tsub) => sup match {
        case Nil => false
        case Cons(hsup, tsup) =>
          startsWith(sup, sub) || hasSubsequence(tsup, sub)
      }
    }
  }
}
