import Stream._
sealed trait Stream[+A] {
  def toList: List[A] = this match {case Empty => Nil ;case Cons(h, t) => h() :: t().toList}
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  def exists2(p: A => Boolean): Boolean =
    foldRight(false)((a,b) => p(a) || b)
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h,t) => Some(h())
  }
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case Cons(h, t) if !p(h()) => empty
    case _ => empty
  }
  def takeWhile2(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else b)
  def headOption2: Option[A] =
    foldRight(None: Option[A])((a,b) => Some(a))
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty:Stream[B])((a,b) => cons(f(a),b))
  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a,b) => if (p(a)) b else cons(a,b))
  def append[B >: A](as: Stream[B]): Stream[B] =
    foldRight(as)((a,b) => cons(a,b))
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((a,b) => f(a).append(b))
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
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
object Stream {def apply[A](as: A*): Stream[A] =if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {lazy val head = hd ; lazy val tail = tl ; Cons(() => head, () => tail)}
  def empty[A]: Stream[A] = Empty}
val x = Stream(1,2,3,4)
x.forAll(_ < 3)
x.takeWhile2(_ < 4).toList
x.headOption2
val y = Stream()
y.headOption2
x.map(x => {println(s"x: ${x}"); 4*x}).toList
x.filter(_ == 2).toList
val z = Stream("happy","go","lucky")
x.map(_.toString).append(z).takeWhile(_ != "go").toList
x.flatMap(z => cons(z * 2,cons(z*4,Empty))).toList
def ones(r: Int): Stream[Int] = Stream.cons({println(s"yup: ${r}"); r}, ones(r+1))
ones(1).take(5).toList
ones(1).map(_ + 1).exists(_ % 9 == 0)
ones(1).forAll(_ < 0)
val fibs: Stream[Int] = {
  def fib(a: Int, b: Int): Stream[Int] = {
    Stream.cons(b, fib(b, a + b))
  }
  Stream.cons(0, fib(0, 1))
}
fibs.take(8).toList
def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  f(z) match {
    case None => Empty
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
  }
val fibs2: Stream[Int] =
  unfold((0,1))(x => Some((x._1, (x._2,x._1 + x._2))))
fibs2.take(8).toList
def from2(n: Int): Stream[Int] = {
  unfold(n)(x => Some((x, x + 1)))
}
from2(8).take(4).toList
def constant(k: Int): Stream[Int] = unfold(k)(x => Some((k,k)))
val ones2: Stream[Int] = unfold(1)(x => Some((1,1)))


