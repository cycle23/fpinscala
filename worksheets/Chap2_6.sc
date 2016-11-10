def partial1[A,B,C](a: A, f: (A, B) => C): B => C =
  (b: B) => f(a,b)

def curry[A,B,C](f: (A, B) => C): A => (B => C) =
  (a: A) => (b: B) => f(a,b)

val c = curry((x: Int, y:Int) => x > y)
c(1)
c(2)(1)

val p = partial1(1, (x: Int, y:Int) => x > y)
p(2)
p(0)

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a: A, b: B) => f(a)(b)

val uc = uncurry(c)

uc(2,1)
uc(1,2)

def compose[A,B,C](f: B => C, g: A => B): A => C =
  (a: A) => f(g(a))

val nonEmpty = compose((x: Int) => x > 0, (s: String) => s.length)
nonEmpty("Cody")
nonEmpty("")

