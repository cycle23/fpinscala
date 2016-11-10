sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = { this match { case None => None
      case Some(x) => Some(f(x)) }}
  def getOrElse[B >: A](default: => B): B = {this match { case None => default
      case Some(x) => x}}
  def filter(f: A => Boolean): Option[A] = { if (map(f).getOrElse(false)) this
    else None}
  def flatMap[B](f: A => Option[B]): Option[B] = { map(f).getOrElse(None)}
  def orElse[B >: A](ob: => Option[B]): Option[B] = { this match { case None => ob
      case Some(x) => Some(x) } } }
case class Some[A](get: A) extends Option[A]
case object None extends Option[Nothing]
object Option { def apply[A](x: A): Option[A] = Some(x)}
case class Employee(name: String, department: String)
def lookupByName(name: String): Option[Employee] = Option(Employee("Joe","Account"))
def lookupByDepartment(dept: Option[String]): Option[Employee] = Option(Employee("Joe","fark"))
//val joeDepartment: Option[String] = lookupByName("Joe").flatMap(x => lookupByDepartment(x.department))

def mean(xs: Seq[Double]): Option[Double] = {
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)
}
def variance(xs: Seq[Double]): Option[Double] = {
  mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x-m, 2))))
}

val xs = Seq(2.0,3.0,4.0,Double.NaN)
val ys = Seq()
variance(xs)
variance(ys)

val dept: String =
  lookupByName("Joe").map(_.department).
    filter(_ != "Account").getOrElse("Default Dept")

