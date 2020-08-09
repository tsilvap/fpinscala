package fpinscala.errorhandling

sealed trait Option[+A] {
  // Exercise 4.1
  def map[B](f: A => B): Option[B] =
    this match {
      case None    => None
      case Some(a) => Some(f(a))
    }

  def getOrElse[B >: A](default: => B): B =
    this match {
      case None    => default
      case Some(a) => a
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this.map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    if (this != None) this else ob

  def filter(f: A => Boolean): Option[A] =
    this match {
      case None    => None
      case Some(a) => if (f(a)) Some(a) else None
    }
  //\4.1
}

object Option {
  // Exercise 4.2
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m =>
      if (xs.isEmpty) None
      else Some(xs.map(x => math.pow(x - m, 2)).sum / xs.length)
    )
  //\4.2

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch { case e: Exception => None }

  // Exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    (a, b) match {
      case (None, _)          => None
      case (_, None)          => None
      case (Some(a), Some(b)) => Some(f(a, b))
    }
  def map2_v2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap (aa => b map (bb => f(aa, bb)))
  //\4.3

  // Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    if (a contains None) None
    else
      Some(
        a map (a =>
          a match {
            case Some(a) => a
            case None    => throw new Exception()
          }
        )
      )
  /* Recursive version */
  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    a match {
      case Nil    => Some(Nil)
      case h :: t => h flatMap (hh => sequence2(t) map (hh :: _))
    }
  /* Using foldRight */
  def sequence3[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((x, y) => map2(x, y)(_ :: _))
  //\4.4

  // Exercise 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a match {
      case Nil    => Some(Nil)
      case h :: t => f(h) flatMap (hh => traverse(t)(f) map (hh :: _))
    }

  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
     a.foldRight[Option[List[B]]](Some(Nil))((h,t) => map2(f(h),t)(_ :: _))   

  def sequenceViaTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(x => x)
  //\4.5
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]
