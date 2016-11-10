trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}
def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B = {
  as.foldLeft(m.zero)((b: B, a: A) => m.op(f(a), b))
}
def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
  if (v.isEmpty)
    m.zero
  else if (v.length == 1) {
    println("1 case...")
    val g= m.op(m.zero, f(v(0)))
    println(s"got: ${g}")
    g
  }
  else if (v.length == 2) {
    println("2 case...")
    val g = m.op(f(v(0)), f(v(1)))
    //val g = f(m.op(v(0),v(1)))
    println(s"got: ${g}")
    g
  }
  else {
    val mid = v.length / 2
    println(s"mid: ${mid}...")
    val (front, back) = v.splitAt(mid)
    m.op(foldMapV(front, m)(f),foldMapV(back, m)(f))
  }
}
val intAddition = new Monoid[Int] {
  def op(a1: Int, a2: Int) = a1 + a2
  val zero = 0
}
val pairCompare = new Monoid[(Int,Boolean)] {
  def op(a1: (Int, Boolean), a2: (Int,Boolean)) = if (a1._1 <= a2._1) (a2._1, a1._2 && a2._2) else (a2._1, false)
  def zero = (Int.MinValue,true)
}
val booleanOr = new Monoid[Boolean] {
  def op(a1: Boolean, a2: Boolean) = a1 || a2
  def zero = false
}
def endoMonoid[A] = new Monoid[A => A] {
  def op(a1: A => A, a2: A => A) = a1.andThen(a2)
  def zero = A => A
}

/*
def compareMon = new Monoid[Int => Boolean] {
  def op(a1: Int => Boolean, a2: Int => Boolean): Boolean =
  def zero = () => true
}
*/
val v = IndexedSeq(Int.MinValue,1,2,3,4,5,6,7,8,9)
foldMapV(v, intAddition)(x => x)
foldMapV(v, pairCompare)(x => (x, true))._2
//foldMapV(v, compareThem)((x,x) => false)
booleanOr.op(true, false)
//compareThem.op(1, 2)

