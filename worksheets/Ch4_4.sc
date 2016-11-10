sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) =>
        Right(f(a))
    }
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    map(f) match {
      case Left(e) => Left(e)
      case Right(a) => a
    }
  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] =
    this match {
      case Left(e) => b
      case Right(a) => this
    }
  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = {
    this match {
      case Left(e) => Left(e)
      case Right(a) => b match {
        case Left(ee) => Left(ee)
        case Right(bb) =>
          Right(f(a,bb))
      }
    }
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

def mean(xs: IndexedSeq[Double]): Either[String, Double] =
  if (xs.isEmpty)
    Left("mean of tempty pist!")
  else
    Right(xs.sum / xs.length)

def safeDiv(x: Int, y: Int): Either[Exception, Int] =
  try Right(x / y)
  catch { case e: Exception => Left(e) }

def Try[A](a: => A): Either[Exception, A] =
  try Right(a)
  catch { case e: Exception => Left(e) }

val j = mean(Vector())
val i = mean(Vector(1.0))
val h = mean(Vector(1.0,2.0))
def by2(x: Double): Int = { (x * 2.0).toInt }
def multToString(x: Double, y: Int): String = {
  (x * y).toString
}
def makeString(x: Double): Either[Exception, String] = {
  Try(x.toString)
}
val k = j.map(by2)
val k2= i.map(by2)
val l = j.map2(i.map(by2))(multToString)
val l2 = h.map2(i.map(by2))(multToString)
val m = k.orElse(l)
val m2 = k.orElse(l2)
val m3 = l2.orElse(k)

val n = j.flatMap(makeString)
val o = i.flatMap(makeString)
val o2 = safeDiv(2,0).flatMap(x => Try(x.toDouble)).flatMap(makeString)

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

sequence(List(k2, l2, m2, m3, o))
sequence(List(k2, l2, m2, k, m3, o, o2))
sequence(List(o2, k2, l2, m2, k, m3, o, o2))
traverse(List("1","2.0","fark",""))(x => Try(x.toDouble))
traverse(List("fark","2.0","fark",""))(x => Try(x.toDouble))
traverse(List("1","NaN","2.0"))(x => Try(x.toDouble))
