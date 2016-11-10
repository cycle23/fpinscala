sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match { case Left(e) => Left(e)
      case Right(a) => Right(f(a))}
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = map(f) match { case Left(e) => Left(e)
      case Right(a) => a}
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {case Left(e) => b
      case Right(a) => this}
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {this match {case Left(e) => Left(e)
      case Right(a) => b match { case Left(ee) => Left(ee)
        case Right(bb) => Right(f(a,bb))}}}}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]
def Try[A](a: => A): Either[Exception, A] = try Right(a) catch { case e: Exception => Left(e) }
def insuranceRateQuote(age: Int, numberOfSpeedingTickeets: Int): Double = {3.0}
def parseInsuranceRateQuote(age: String,
                            numberOfSpeedingTickets: String): Either[Exception, Double] =
  for {
    a <- Try { age.toInt }
    tickets <- Try { numberOfSpeedingTickets.toInt }
  } yield insuranceRateQuote(a, tickets)
def traverse[E, A, B](a: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
  a.foldRight(Right(List()): Either[E, List[B]])((aa: A, bb: Either[E, List[B]]) =>
    f(aa) match {
      case Left(e) => Left(e)
      case Right(bbb) => bb match {
        case Left(e) => Left(e)
        case Right(x) => Right(bbb :: x)
      }
    }
  )
}
def sequence[E, A](a: List[Either[E, A]]): Either[E, List[A]] =
  traverse[E, Either[E, A], A](a)((aa) => aa)
