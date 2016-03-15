import RNG.{Rand, Simple}

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
  assert (RNG.intDouble(rng) == ((16159453, 1.281479696E9), Simple(197491923327988L)), "RNG.intDouble doesn't work")

  // doubleInt
assert (RNG.doubleInt(rng) == ((1.281479696E9, 16159453),Simple(197491923327988L)), "RNG.doubleInt doesn't work")

  // ints
  assert (RNG.ints(0)(rng) == (List(), Simple(42L)), "RNG.ints doesn't work")
  assert (RNG.ints(3)(rng) == (List(16159453,-1281479697 ,-340305902), Simple(259172689157871L)), "RNG.ints doesn't work")

  // _double
  assert (RNG._double(rng) == (16159453d,Simple(1059025964525L)), "RNG.double doesn't work")

  // map2
  val rngMap2 = RNG.map2 (RNG.int, RNG.int) (_+_)
  assert (rngMap2 (rng) == (-1265320244,Simple(197491923327988L)), "map2 doesn't work")

  // sequence
  val lr = List( RNG.int, RNG.int, RNG.int )
  val e = RNG.sequence (lr) (rng)
  assert (e == (List(16159453,-1281479697 ,-340305902), Simple(259172689157871L)), "sequence doesn't work")

  // flatMap
  val f1: RNG.Rand[Int] = _.nextInt
  val g1: RNG.Rand[Double] = rng => {
    val (x,r) = rng.nextInt
    (x.toDouble, r)
  }

  val flatM: Rand[Double] = RNG.flatMap[Int, Double] (f1)(i => g1)
  assert (flatM(rng) == (-1.281479697E9,Simple(197491923327988L)))

  // nonNegativeLessThan
  assert (RNG.nonNegativeLessThan(2)(rng) == (1,Simple(1059025964525L)), "nonNegativeLessThan doesn't work")

  // _map2
  val _rngMap2 = RNG._map2 (RNG.int, RNG.int) (_+_)
  assert (_rngMap2 (rng) == (-1265320244,Simple(197491923327988L)), "map2 doesn't work")

  val s1 = new State[String, Int](s => (s.toInt, s))
  val s2 = new State[String, Int](s => (2, ""))
//  println( s1.flatMap[Int] ((i: Int) => s2).run("") )
//  println( s1.run ("") )

  val s2i = new State[String,Int](str => (str.toInt, str))
  val i2d = new State[Int,Double](i => (i.toDouble, i))
  val s2d = new State[String,Double](str => s2i.map[Double](_.toDouble).run(str))
  assert (s2d.run("1") == (1.0,"1"), "State.map doesn't work")

  val s2d_ = s2i.flatMap[Double](_ => s2d)
  assert (s2d_.run("1") == (1.0,"1"), "State.flatMap doesn't work")

  val inc = new State[Int,Int](i => (i+1, i+1))

  /* The following should:
   * > i+1
   * > i+1 - yes, twice!
   * > add (i+1) + (i+1)
   * > convert it to a Double
   */
  val ii2d = inc.map2[Int,Double] (inc) ((i1,i2) => (i1 + i2).toDouble)

  /* Setting i=0
   * > 0+1 = 1
   * > 1+1 = 2
   * > add 1 + 2
   * > convert to Double = 3.0
   * (3.0, state = 2)
   */
  assert (ii2d.run (0) == (3.0,2), "State.map2 doesn't work")

  // State.sequence
  val slr = List( RNG.int, RNG.int, RNG.int )
  val se = RNG.sequence (lr) (rng)

  assert (se == (List(16159453,-1281479697 ,-340305902), Simple(259172689157871L)), "sequence doesn't work")

  // state2stream
//  type Rand[A] = State[RNG, A]
  val rng1: State[RNG, Int] = new State[RNG,Int](rng => rng.nextInt)

  val s2s = State.state2stream[RNG, Int] (rng1) (rng)
  assert (s2s.take(3).toList == List(16159453,-1281479697 ,-340305902), "state2stream doesn't work")
  assert (s2s.take(2).toList == List(16159453,-1281479697), "state2stream doesn't work")
  assert (s2s.take(0).toList == List(), "state2stream doesn't work")

  // random_integers
  assert (State.random_integers.take(3) == List(16159453,-1281479697 ,-340305902), "random_integers didn't work")

  // Candy machine
  val machine = Machine(locked = false, 5, 10)
  val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
  val (out, m2) = Candy.simulateMachine(inputs).run(machine)
  assert (out == (14,1), "didn't simulate machine properly")
}
