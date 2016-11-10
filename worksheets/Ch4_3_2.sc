def Try[A](a: => A): Option[A] =
  try Some(a)
  catch { case e: Exception => None }
def lift[A,B](f: A=> B): Option[A] => Option[B] = _ map f
val abs0: Option[Double] => Option[Double] = lift(math.abs)
abs0(Try("Farkme".toDouble))
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
  for {
    x <- a
    y <- b
  } yield f(x,y)
}
def insuranceRateQuote(age: Int, numberOfSpeedingTickeets: Int): Double = {
  3.0
}
def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): Option[Double] = {
  val optAge: Option[Int] = Try(age.toInt)
  val optTickets: Option[Int] = Try(numberOfSpeedingTickets.toInt)
  map2(optAge, optTickets)(insuranceRateQuote)
}
parseInsuranceRateQuote("nope","1")
parseInsuranceRateQuote("1","nope")
parseInsuranceRateQuote("1","2")
def sequence[A](a: List[Option[A]]): Option[List[A]] = {
  a.foldRight(Some(List()): Option[List[A]])((y: Option[A],x: Option[List[A]]) =>
    y match {
      case None => None: Option[List[A]]
      case Some(b) => x match {
        case None => None
        case Some(c) => Some(b :: c)
      }
    })
}
val a = List(Some(1),Some(2))
sequence(a)
def parseInts(a: List[String]): Option[List[Int]] =
  sequence(a map (i => Try(i.toInt)))
def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
  a.foldRight(Some(List()): Option[List[B]])((aa: A, bb: Option[List[B]]) =>
    f(aa) match {
      case None => None: Option[List[B]]
      case Some(bbb) => bb match {
        case None => None
        case Some(x) => Some(bbb :: x)
      }
    }
  )
}
def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse[Option[A],A](a)((aa) => aa)
parseInts(List("1","2"))
parseInts(List())
parseInts(List("1","string"))
parseInts(List("String","2"))
traverse(List("1","String"))(i => Try(i.toInt))
traverse(List("1","2"))(i => Try(i.toInt))
sequence2(List(Some(1),Some(2)))
sequence2(List(None,Some(3)))
