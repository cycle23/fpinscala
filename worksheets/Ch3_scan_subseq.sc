List(1,2,3).scanLeft(0)(_ + _)
List(1,2,3).scanRight(0)(_ + _)

def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
  def checkSeq(sup: List[A], subMatching: List[A]): Boolean = {
    (sup,subMatching) match {
      // ate all of subMatching list
      case (_,Nil) => true
      // ate all input but still had non-matched items
      case (Nil, _) => false
      case (a :: as,x :: xs) =>
        if (a !=x ) // restart match pattern
          checkSeq(as, sub)
        else
          checkSeq(as, xs)
    }
  }
  checkSeq(sup, sub)
}

hasSubsequence(List(1,2,3,1,2,1,2,2,3,2),List(2,2))
