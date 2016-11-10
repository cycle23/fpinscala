def fibonacci(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, i: Int, last: Int, acc: Int): Int = {
    if (i == n) acc
    else if (i == 0) go(n, i + 1, 0, 0)
    else if (i == 1) go(n, i + 1, 0, 1)
    else go(n, i + 1, acc, last + acc)
  }
  go(n, 0, 0, 0)
}


fibonacci(1)
fibonacci(2)
fibonacci(3)
fibonacci(4)
fibonacci(5)
fibonacci(6)
fibonacci(7)


def factorial(n: Int): Int = {
  @annotation.tailrec
  def go(n: Int, acc: Int): Int =
    if (n <= 0) acc
    else go(n-1, n*acc)

  go(n, 1)
}


