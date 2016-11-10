def if2[A](cond: Boolean, onTrue: () => A, onFalse: () => A): A = if (cond) onTrue() else onFalse()

val a = 23
if2(a<22, () => println("a"), () => println("b"))

def if2_2[A](cond: Boolean, onTrue: => A, onFalse: => A): A =
  if (cond) onTrue else onFalse

if2_2(false, sys.error("fail"), 3)

def maybeTwice(b: Boolean, i: => Int) = {
  println("howdy")
  if (b) i + i else 0
}

val x = maybeTwice(true, {println("hi"); 1 + 41})

def maybeTwice2(b: Boolean, i: => Int) = {
  lazy val j = i
  println("eval")
  if (b) j+j else 0
}

val y = maybeTwice2(true, {println("ho"); 1+22})
