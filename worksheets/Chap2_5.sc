def findFirst[A](as: Array[A], p:A=>Boolean): Int = {
  @annotation.tailrec
  def loop(n: Int): Int =
    if (n >= as.length) -1
    else if(p(as(n))) n
    else loop(n+1)

  loop(0)
}
findFirst(Array(7,9,13), (x: Int) => x == 9)

def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
  @annotation.tailrec
  def loop(n: Int, last: A): Boolean =
    if (n >= as.length) true
    else if(n >= 1 && !ordered(last, as(n))) false
    else loop(n +1, as(n))

  if (as.isEmpty) true
  else loop(1, as(0))
}

isSorted[Int](Array(1,3,4,5,1), (x, y) => x < y)
