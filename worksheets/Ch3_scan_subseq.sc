List(1,2,3).scanLeft(0)(_ + _)
List(1,2,3).scanRight(0)(_ + _)

def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
  // currently wrong, needs to consider contiguous subsequence, this allows gaps
  def checkSeq(sup: List[A], sub: List[A]): Boolean = {
    (sup,sub) match {
      case (_,Nil) => true
      case (Nil, _) => false
      case (s, x :: xs) =>
        println(x)
        val sup2 = s.takeWhile((y) => x != y)
        if (sup2.length == s.length) false
        else {
          s.drop(sup2.length) match {
            case (s3Head :: s3Tail) if s3Head == x => checkSeq(s3Tail, xs)
            case _ => false
          }
        }
        //println(s3)

    }
  }
  checkSeq(sup, sub)
}

hasSubsequence(List(1,2,3,1,2),List(2,3,2))


