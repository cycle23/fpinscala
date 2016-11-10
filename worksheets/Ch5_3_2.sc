import Stream._
sealed trait Stream[+A] {
  def toList: List[A] = this match {case Empty => Nil ;case Cons(h, t) => h() :: t().toList}
  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match { case Cons(h, t) => f(h(), t().foldRight(z)(f)); case _ => z}
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case Cons(h, t) if !p(h()) => empty
    case _ => empty
  }
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty:Stream[B])((a,b) => cons(f(a),b))
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }
  def take2(n: Int): Stream[A] = unfold((this,n)) {
    case (s: Stream[A], n: Int) => s match {
      case Cons(h, t) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None
    }
  }
  def map2[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }
  def takeWhile2(p: A => Boolean): Stream[A] =
    unfold(this){
      case Cons(h, t) =>
        if (p(h())) Some((h(), t()))
        else None
      case _ => None
    }
  def zipWith[AA >: A](as: Stream[AA])(f: (AA, AA) => AA): Stream[AA] =
    unfold((this: Stream[AA], as)){
      case (Cons(x, xs),Cons(y,ys)) => Some((f(x(), y()),(xs(), ys())))
      case (_, _) => None
    }
  def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold((this, s2)){
      case (Cons(x, xs),Cons(y,ys)) => Some(
        (
          (Some(x()),Some(y())),
          (xs(),ys()))
        )
      case (Cons(x,xs),_) => Some(
        (
          (Some(x()),None),
          (xs(),Empty)
        )
      )
      case (_,Cons(y,ys)) => Some(
        (
          (None,Some(y())),
          (Empty,ys())
        )
      )
      case (_,_) => None
    }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
object Stream {def apply[A](as: A*): Stream[A] =if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {lazy val head = hd ; lazy val tail = tl ; Cons(() => head, () => tail)}
  def empty[A]: Stream[A] = Empty}
def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  f(z) match {
    case None => Empty
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
  }
val fibs: Stream[Int] =
  unfold((0,1))(x => Some((x._1, (x._2,x._1 + x._2))))
fibs.take2(8).toList
fibs.map2(_ * 2).take2(10).toList
fibs.takeWhile2(_ < 90).toList
def from2(n: Int): Stream[Int] = {
  unfold(n)(x => Some((x, x + 1)))
}
fibs.zipWith(from2(3))(_+_).take2(8).toList
fibs.take2(7).zipAll(from2(80)).take2(20).toList
