import Stream._
sealed trait Stream[+A] {
  def headOption: Option[A] =
    foldRight(None: Option[A])((a,b) => Some(a))
  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a,b) => p(a) || b)
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }
  def take(n: Int): Stream[A] = unfold((this,n)) {
    case (s: Stream[A], n: Int) => s match {
      case Cons(h, t) if n > 0 => Some((h(), (t(), n - 1)))
      case _ => None
    }
  }
  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => t().drop(n-1)
    case Cons(h, t) if n == 1 => t()
    case _ => empty
  }
  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a, b) => if (p(a)) cons(a, b) else b)
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)
  def map[B](f: A => B): Stream[B] =
    foldRight(Empty:Stream[B])((a,b) => cons(f(a),b))
  def filter(p: A => Boolean): Stream[A] =
    foldRight(Empty: Stream[A])((a,b) => if (p(a)) b else cons(a,b))
  def append[B >: A](as: Stream[B]): Stream[B] =
    foldRight(as)((a,b) => cons(a,b))
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Empty: Stream[B])((a,b) => f(a).append(b))
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
  def startsWith[A](s: Stream[A]): Boolean =
    (this,s) match {
      case (_,Empty) => true
      case (Cons(h, t),Cons(h2,t2)) if h() == h2() => t().startsWith(t2())
      case _ => false
    }
  def startsWith2[A](s: Stream[A]): Boolean =
    this.zipAll(s).foldRight(true)((a,b) => a match {
      case (Some(x),Some(y)) => b && (x == y)
      case _ => false
    })
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case (Cons(x,xs)) => Some((cons(x(),xs()),xs()))
      case _ => None
    }
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    this match {
      case Empty => cons(z,Empty)
      case Cons(h,t) => {
        val tail = t().scanRight(z)(f)
        cons(f(h(),tail.headOption.get),tail)
      }
    }

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith2 s)
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
def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
  f(z) match {
    case None => Empty
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
  }
val x = Stream(1,2,3,4)
val y = Stream(1,2,4)
val z = x.tails
val zz = y.tails
z.toList.map(_.toList)
zz.toList.map(_.toList)
Stream(1,2,3,1,2,1,2,2,3,2).hasSubsequence(Stream(2,3,2))
Stream(1,2,3).scanRight(0)(_ + _).toList
Stream(1,2,3).scanRight(Empty: Stream[Int])(cons(_,_)).toList.map(_.toList)