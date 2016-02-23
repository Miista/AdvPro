import fpinscala.laziness.Stream._
import fpinscala.laziness.{Empty, Stream}

/**
  * Created by palmund on 17/02/2016.
  */
object Tests extends App {
  // this is how we do simple interactive testing
  val l1 = Empty
  val l2: Stream[Int] = empty

  val l3: Stream[Int] = cons(1, cons(2, cons(3, empty)))

  println(l1.headOption)
  println(l2.headOption)
  println(l3.headOption)

  val l4 = Stream.to(2)
  assert (l4.headOption() == Some(2), "!= 2")
  assert (l4.tail.headOption() == Some(1), "!= 1")
  assert (l4.tail.tail.headOption() == Some(0), "!= 0")

  val l5 = Stream.from(0)
  assert (l5.headOption() == Some(0), "!= 0")
  assert (l5.tail.headOption() == Some(1), "!= 1")
  assert (l5.tail.tail.headOption() == Some(2), "!= 2")

  val fs1 = Stream(1,2,3,4)
  assert (fs1.toList == List(1, 2, 3, 4))

  val fs2 = Stream(1,2,3,4)
  assert (fs2.take(2).toList == List(1, 2), "Didn't take [1,2] from Stream")

  val fs3 = Stream(1,2,3,4)
  assert (fs3.drop(2).toList == List(3,4), "Didn't drop [1,2] from Stream")
  assert (fs3.drop(1).toList == List(2,3,4), "Didn't drop [1] from Stream")
  assert (fs3.drop(4).toList == List(), "Didn't drop entire Stream")

  val naturals = Stream.from(0)
  naturals.takeWhile(_<1000000000).drop(100).take(50).toList

  assert (naturals.takeWhile(_<5).toList == List(0,1,2,3,4), "Didn't takeWhile")

  naturals.forAll (_ < 0)
//  naturals.forAll (_ >=0) // Should crash

  naturals.takeWhile1(_<1000000000).drop(100).take(50).toList

  assert (naturals.headOption1() == Some(0), "headOption1 != 0")
  assert (naturals.drop(5).headOption1() == Some(5), "headOption1 != 5")
  assert (naturals.drop(5).take(5).drop(4).headOption1() == Some(9), "headOption1 != 9")

  naturals.map (_*2).drop (30).take (50).toList

  naturals.drop(42).filter (_%2 ==0).take (30).toList

  Stream(1,2,3).append(Stream(4,5,6))
  naturals.append (naturals)
  naturals.take(10).append(naturals).take(20).toList

//  naturals.flatMap (to _).take (100).toList
//  naturals.flatMap (x =>from (x)).take (100).toList

  assert (Stream(1,2,3).startsWith (Stream(1,2)), "Stream doesn't start with 1,2")
}
