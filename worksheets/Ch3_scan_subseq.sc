List(1,2,3).scanLeft(0)(_ + _)
List(1,2,3).scanRight(0)(_ + _)

def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
  def checkSeq(sup: List[A], sub: List[A]): Boolean = {
    (sup,sub) match {
      case (_,Nil) => true
      case (Nil, _) => false
      case (s, x :: xs) =>
        //println(x)
        val sup2 = s.takeWhile((y) => x != y)
        if (sup2.length == s.length) return false
        //println(sup2)
        val s3 = sup2 ++ s.takeRight(s.length- (sup2.length + 1))
        //println(s3)
        checkSeq(s3, xs)
    }
  }
  checkSeq(sup, sub)
}

hasSubsequence(List(1,2,3,1),List(1,1,2,1))

