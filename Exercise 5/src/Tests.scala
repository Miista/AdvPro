import RNG.Simple

/**
  * Created by palmund on 01/03/2016.
  */
object Tests extends App {
  val rng: Simple = new RNG.Simple(42)

  // nextInt
  assert (rng.nextInt == (16159453,Simple(1059025964525L)), "rng.nextInt doesn't work")

  // nonNegativeInt
  assert (RNG.nonNegativeInt(rng) == (16159453,Simple(1059025964525L)), "RNG.nonNegativeInt doesn't work")

  // double
  assert (RNG.double(rng) == (16159453d,Simple(1059025964525L)), "RNG.double doesn't work")

  // intDouble
  assert (RNG.intDouble(rng) == ((16159453,16159453d),Simple(1059025964525L)), "RNG.intDouble doesn't work")

  // doubleInt
  assert (RNG.doubleInt(rng) == ((16159453d, 16159453),Simple(1059025964525L)), "RNG.doubleInt doesn't work")

  // ints
  assert (RNG.ints(3)(rng) == (List(16159453,-1281479697 ,-340305902), Simple(259172689157871L)), "RNG.ints doesn't work")
//  val e = rng.nextInt
//  val e1 = e._2.nextInt
//  val e2 = e1._2.nextInt
//  println(e._1)
//  println(e1._1)
//  println(e2._1)
//  println(e2._2)
}
