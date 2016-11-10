sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

}

def tail[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case Cons(h, t) => t
}

def setHead[A](l: List[A], a: A): List[A] = l match {
  case Nil => Cons(a, Nil)
  case Cons(_, t) => Cons(a, t)
}

val v = List(1,2,3)

setHead(v, 3)

def drop[A](l: List[A], n: Int): List[A] =
  if (n == 0) l
  else l match {
    case Nil => Nil
    case Cons(h, t) => drop(t, n - 1)
  }

drop(v, 5)
drop(v, 1)

def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
  l match {
    case Nil => Nil
    case Cons(h, t) =>
      if (!f(h)) l
      else dropWhile(t, f)
  }

dropWhile(v, (x: Int) => x < 2)

def init[A](l: List[A]): List[A] =
  l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) =>
      Cons(h, init(t))
  }

init(v)

def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] =
  l match {
    case Cons(h, t) if f(h) => dropWhile2(t)(f)
    case _ => l
  }

dropWhile2(v)(x => x < 2)

