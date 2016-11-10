sealed trait Tree[+A]
//case class EmptyTree[A] extends Tree[A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
def size[A](t: Tree[A]): Int = {
  t match {
    case Leaf(_) => 1
    case Branch(l, r) =>
      val lz = size(l)
      val rz = size(r)
      lz + rz
  }
}
val t = Branch(Leaf(1),Branch(Branch(Leaf(2),Leaf(3)),Leaf(5)))
def max(t: Tree[Int]): Int = {
  def m(t: Tree[Int], acc: Int): Int = {
    t match {
      case Leaf(v) => v max acc
      case Branch(l,r) => m(l, acc) max m(r,acc)
    }
  }
  m(t, Int.MinValue)
}
size(t)
max(t)
def depth(t: Tree[Int]): Int = {
  def d(t: Tree[Int], acc: Int): Int = {
    t match {
      case Leaf(_) => acc
      case Branch(l, r) => d(l, acc+1) max d(r, acc+1)
    }
  }
  d(t,0)
}
depth(t)
def map[A,B](t: Tree[A])(f: (A) => B): Tree[B] = {
  t match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l)(f),map(r)(f))
  }
}
map(t)(_ * 2)
def fold[A,B](t: Tree[A], z: B)
             (leaf_op: (A,B) => B)
             (branch_op: (B,B) => B): B = {
  t match {
    case Leaf(v) => leaf_op(v,z)
    case Branch(l,r) => branch_op(fold(l,z)(leaf_op)(branch_op),
                                  fold(r,z)(leaf_op)(branch_op))
  }
}
def size2[A](t: Tree[A]): Int = {
  fold(t, 0)((_,v) => 1)(_ + _)
}
size2(t)
def max2(t: Tree[Int]): Int = {
  fold(t, Integer.MIN_VALUE)(_ max _)(_ max _)
}
max2(t)
def depth2(t: Tree[Int]): Int = {
  fold(t, 0)((_, v) => v)((x,y) => x + 1 max y + 1)
}
depth2(t)
val t2 = Leaf(1)
depth2(t2)
val t3 = Branch(Leaf(1),Leaf(2))
depth2(t3)

case object EmptyTree extends Tree[Nothing]
def map2[A,B](t: Tree[A])(f: (A) => B): Tree[B] = {
  def g(a: A, b: Tree[B]): Tree[B] = Leaf(f(a))
  fold(t,EmptyTree: Tree[B])(g)((a: Tree[B], b: Tree[B]) => Branch(a,b))
}
map2(t)(_ * 3)
