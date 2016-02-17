import fpinscala.laziness.Stream._
import fpinscala.laziness.{Empty, Stream}

/**
  * Created by palmund on 17/02/2016.
  */
object Tests extends App {
  // this is how we do simple interactive testing
  val l1: Stream[Int] = Empty
  val l2: Stream[Int] = empty

  val l3: Stream[Int] = cons(1, cons(2, cons(3, empty)))

  println(l1.headOption)
  println(l2.headOption)
  println(l3.headOption)

//  val l4 = Stream.to(2)
//  assert (l4.headOption() == Some(0), "!= 0")
//  assert (l4.tail.headOption() == Some(1), "!= 1")
//  assert (l4.tail.tail.headOption() == Some(2), "!= 2")

  val l5 = Stream.from(0)
  assert (l5.headOption() == Some(0), "!= 0")
  assert (l5.tail.headOption() == Some(1), "!= 1")
  assert (l5.tail.tail.headOption() == Some(2), "!= 2")
}
