sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}
@annotation.tailrec
def foldLeft[A,B](as: List[A], z:B)(f: (B, A) => B): B = as match { case Nil => z; case Cons(x, xs) => foldLeft(xs, f(z, x))(f)}
def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = { def reverse(as: List[A], z: List[A]): List[A] = as match {case Nil => Nil; case Cons(x, Nil) => Cons(x, z); case Cons(x, xs) => reverse(xs, Cons(x, z))}
  def g(b: B, a: A): B = f(a, b); foldLeft(reverse(as, Nil), z)(g)
}
def add1(as: List[Int]): List[Int] =
  foldRight2(as, Nil: List[Int])((x, xs) => Cons(x+1,xs))
add1(List(1,2,3))
def dbl2Str(as: List[Double]): List[String] =
  foldRight2(as, Nil: List[String])((x, xs) => Cons(x.toString, xs))
dbl2Str(List(1.0,0.0,4.0))
def map[A, B](as: List[A])(f: A => B): List[B] =
  foldRight2(as, Nil: List[B])((x, xs) => Cons(f(x),xs))
map(List(1,2,3))((x:Int) => x * 2.0)
def filter[A](as: List[A])(f: A => Boolean): List[A] =
  foldRight2(as, Nil: List[A])((x, xs) =>
    if (f(x)) Cons(x, xs)
    else xs
  )
filter(List(1,2,3))((x: Int) => (x % 2) == 0)
def append[A](as: List[A], at: List[A]): List[A] = {
  foldRight2(as, at)((x, xs) => Cons(x, xs))
}
def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
  foldRight2(as, Nil: List[B])((x, xs) => append(f(x),xs))
flatMap(List(1,2,3))(i => List(i,i))
def filter2[A](as: List[A])(f: A => Boolean): List[A] =
  flatMap(as)((x) => if (f(x)) List(x) else Nil)
filter2(List(1,2,3))((x: Int) => (x % 2) == 0)
def zipIntSums(as: List[Int], at: List[Int]): List[Int] =
  (as, at) match {
    case (Nil, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, zipIntSums(xs, ys))
  }
zipIntSums(List(1,2,3),List(4,5,6))
def zipWith[A](as: List[A], at: List[A])(f: (A,A) => A): List[A] =
  (as, at) match {
    case (Nil, Nil) => Nil
    case (Cons(x, xs), Nil) => Cons(x, xs)
    case (Nil, Cons(y, ys)) => Cons(y, ys)
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x,y), zipWith(xs, ys)(f))
  }

zipWith(List(2.0,3.0,4.0),List(4.0,5.0,6.0))((x,y) => x * y)