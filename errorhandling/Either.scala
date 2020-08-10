package fpinscala.errorhandling

sealed trait Either[+E, +A] {
  // Exercise 4.6
  def map[B](f: A => B): Either[E, B] =
    this match {
      case l: Left[E] => l
      case Right(a)   => Right(f(a))
    }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case l: Left[EE] => l
      case Right(a)    => f(a)
    }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(_)     => b
      case r: Right[A] => r
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this flatMap (aa => b map (bb => f(aa, bb)))
  //\4.6
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  // Exercise 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight(Right(Nil): Either[E, List[A]])((e, acc) =>
      e.map2(acc)(_ :: _)
    )

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight(Right(Nil): Either[E, List[B]])((e, acc) =>
      f(e).map2(acc)(_ :: _)
    )
  //\4.7

  // Exercise 4.8
  /* Either[List[A], B] would do the trick. */
  //\4.8
}
