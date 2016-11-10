sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]
object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}
def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
  as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }
foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_))
def length[A](as: List[A]): Int =
  foldRight(as, 0)((_,count) => count + 1)
length(List(1,2,3))
length(List(1,2,3,4))
@annotation.tailrec
def foldLeft[A,B](as: List[A], z:B)(f: (B, A) => B): B =
  as match {
    case Nil => z
    case Cons(x, xs) =>
      foldLeft(xs, f(z, x))(f)
  }
foldLeft(List(1,2,3), Nil:List[Int])((y, x) => Cons(x, y))
def length2[A](as: List[A]): Int =
  foldLeft(as, 0)((count, _) => count + 1)
length2(List(1,2,3,4))
def sum(as: List[Int]): Int =
  foldLeft(as, 0)((y, x) => x + y)
sum(List(2,0,2))
def product(as: List[Int]): Int =
  foldLeft(as, 1)((y, x) => x * y)
product(List(2,1,3))
def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
  def reverse(as: List[A], z: List[A]): List[A] =
    as match {
      case Nil => Nil
      case Cons(x, Nil) => Cons(x, z)
      case Cons(x, xs) =>
        reverse(xs, Cons(x, z))
    }
  def g(b: B, a: A): B = f(a, b)
  val ab = reverse(as, Nil)
  val list = foldLeft(ab, z)(g)
  list
}
foldRight2(List(1,2,3), Nil:List[Int])(Cons(_,_))
def append[A](as: List[A], at: List[A]): List[A] = {
  foldRight2(as, at)((x, xs) => Cons(x, xs))
}
append(List(1,2,3),List(4,5,6))
/*
object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}
*/
def appendX[A](as: List[A]*): List[A] =
  if (as.isEmpty) Nil: List[A]
  else append(as.head, appendX(as.tail: _*))

appendX(List(1,2,3),List(4,5,6),List(7,8,9))
