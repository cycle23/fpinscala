trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

val stringMonoid = new Monoid[String] {
  def op(a1: String, a2: String) = a1 + a2
  val zero = ""
}

def listMonoid[A] = new Monoid[List[A]] {
  def op(a1: List[A], a2: List[A]) = a1 ++ a2
  val zero = Nil
}

val intAddition = new Monoid[Int] {
  def op(a1: Int, a2: Int) = a1 + a2
  val zero = 0
}

val intMultiplication = new Monoid[Int] {
  def op(a1: Int, a2: Int) = a1 * a2
  val zero = 1
}

val booleanOr = new Monoid[Boolean] {
  def op(a1: Boolean, a2: Boolean) = a1 || a2
  def zero = false
}

def booleanAnd = new Monoid[Boolean] {
  def op(a1: Boolean, a2: Boolean) = a1 && a2
  def zero = true
}

def optionMonoid[A] = new Monoid[Option[A]] {
  def op(a1: Option[A], a2: Option[A]) = a1.orElse(a2)
  def zero = None
}

def endoMonoid[A] = new Monoid[A => A] {
  def op(a1: A => A, a2: A => A) = a1.andThen(a2)
  def zero = A => A
}

val o1: Option[Int] = Some(1)
val o2: Option[Int] = None
val o3: Option[Int] = Some(3)

optionMonoid.op(o1, o2)
optionMonoid.op(o2, o3)
optionMonoid.op(o1, optionMonoid.op(o2, o3))
optionMonoid.op(optionMonoid.op(o1, o2), o3)

