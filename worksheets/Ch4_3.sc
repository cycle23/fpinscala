sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(x) => Some(f(x))
    }
  }
  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(x) => x
    }
  }
  def filter(f: A => Boolean): Option[A] = {
    if (map(f).getOrElse(false))
      this
    else
      None
  }
  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f).getOrElse(None)
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    this match {
      case None => ob
      case Some(x) => Some(x)
    }
  }
}
case class Some[A](get: A) extends Option[A]
case object None extends Option[Nothing]
object Option {
  def apply[A](x: A): Option[A] =
    Some(x)
}
val f = Option(1)
val g: Option[Int] = None
val h = Option(2)
g.flatMap(x => Some(x*2))
h.flatMap(x => Some(x*2))
g.map(x => x * 2)
f.map(x => x * 2)
g.getOrElse(1)
g.orElse(f)
f.orElse(g)
f.orElse(h)
f.flatMap(x => Some({println(x); x * 2}))
