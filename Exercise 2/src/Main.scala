object Main {
  def main(args: Array[String]) {
    val l: List[Int] = List(1,2,3,1,4)
    assert( List.hasSubsequence(l, List(2,3)) )
    assert( !List.hasSubsequence(l, List(3,4)) )
    assert( List.length(l) == 5 )
    assert( List.length( List.drop(l, 2) ) == 3 )
    def f(i: Int) = i<3
    assert( List.length( List.dropWhile(l, f) ) == 3 )
    assert( List.init(l) == List(1,2,3,1) )
    assert( List.init(List.init(l)) == List(1,2,3) )
//    println(List(1,2,3))
//    println(List.reverse(List(1,2,3)))
    assert( List.reverse(List(1,2,3)) == List(3,2,1) )
    val sum = List.foldRight(l, 0)((v,acc)=>acc+v)
    val sum1 = List.foldRight1(l, 0)((v,acc)=>acc+v)
    assert( sum == sum1 )
  }
}
