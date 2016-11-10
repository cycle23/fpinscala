import Stream._
sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }
  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => t().drop(n-1)
    case Cons(h, t) if n == 1 => t()
    case _ => empty
  }
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case Cons(h, t) if !p(h()) => empty
    case _ => empty
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
object Stream {
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }
  def empty[A]: Stream[A] = Empty
}
object Next {
  var n = 0
  def nextX: Int = {
    n = n + 1
    println(s"n: ${n}")
    n
  }
}
val x = Stream(Next.nextX,
  {println ("2"); 2},
  {println ("3"); 3},
  {println ("4"); 4},
  {println ("5"); 5},
  {println ("6"); 6},
  Next.nextX,
  {println ("8"); 8})
x.toList
x.take(2).toList
x.drop(2).toList
x.takeWhile(_ < 4).toList
