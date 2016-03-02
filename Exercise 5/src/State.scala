trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  // Exercise 1 (CB 6.1)
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, r) = rng.nextInt
    (if (i < 0) -(i + 1) else i, r)
  }

  // Exercise 2 (CB 6.2)
  def double(rng: RNG): (Double, RNG) = {
    val (i,r) = nonNegativeInt(rng)
    (i.toDouble, r)
  }

  // Exercise 3 (CB 6.3)
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r) = nonNegativeInt (rng)
    val (d, r2) = this.double (r)
    val result = (i, d)

    (result, r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (values, r) = intDouble(rng)
    val (i,d) = values
    ((d,i),r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1,r1) = double(rng)
    val (d2,r2) = double(r1)
    val (d3,r3) = double(r2)
    val randoms = (d1,d2,d3)
    (randoms, r3)
  }

  // def boolean(rng: RNG): (Boolean, RNG) =
  //  rng.nextInt match { case (i,rng2) => (i%2==0,rng2) }

  // Exercise 4 (CB 6.4)
  def ints (count: Int)
           (rng: RNG): (List[Int], RNG) =
    if (count == 0) // we should produce NO random ints
      (List.empty[Int], rng)
    else {
      val (x, r) = rng.nextInt
      val (xs, r1) = ints(count-1)(r)
      (x :: xs, r1)
    }

  // There is something terribly repetitive about passing the RNG along
  // every time. What could we do to eliminate some of this duplication
  // of effort?

  type Rand[+A] = RNG => (A, RNG)

  /**
    * Takes a [[RNG]] and produces (&lt;random int&gt;, &lt;next RNG state&gt;)
    */
  val int: Rand[Int] = _.nextInt

  /**
    * Works like [[identity]]
    * @param a
    * @tparam A
    * @return
    */
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // def nonNegativeEven: Rand[Int] = map(nonNegativeInt)(i => i - i % 2)

  // Exercise 5 (CB 6.5)
  val _double: Rand[Double] =
    map[Int,Double] (nonNegativeInt) (_.toDouble)

  // Exercise 6 (CB 6.6)
  def map2[A,B,C] (ra: Rand[A], rb: Rand[B])
                  (f: (A, B) => C): Rand[C] = {
    rng => {
      val (random1, r1) = ra (rng)
      val (random2, r2) = rb (r1)
      val combined = f (random1, random2)
      (combined, r2)
    }
  }

  // this is given in the book

  def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
    map2(ra, rb)((_, _))

  val randIntDouble: Rand[(Int, Double)] = both(int, double)

  val randDoubleInt: Rand[(Double, Int)] = both(double, int)

  // Exercise 7 (6.7)
  def sequence[A] (fs: List[Rand[A]]): Rand[List[A]] = {
    val z: Rand[List[A]] = unit (List.empty[A])
    fs.foldRight[Rand[List[A]]] (z) (map2 (_, _)(_ :: _))
  }


  def _ints(count: Int): Rand[List[Int]] =
    sequence[Int](List.fill (count)(int))

  // Exercise 8 (6.8)
  def flatMap[A,B] (f: Rand[A])
                   (g: A => Rand[B]): Rand[B] =
    rng => {
      val (x,r) = f (rng)
      val (x1,r1) = g (x)(r)
      (x1, r1) // This is just stupid, as we might as well just return g(x)(r)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    }

  // Exercise 9 (6.9)

  // def _map[A,B](s: Rand[A])(f: A => B): Rand[B] =

  // def _map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
}

import State._

case class State[S, +A](run: S => (A, S)) {

  // Exercise 10 (6.10)

  // def map[B](f: A => B): State[S, B] = ???

  // def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = ???

  // def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => ???

}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // Exercise 10 (6.10) continued

  // def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] = ???
  //
  // This is given in the book:

  // def modify[S](f: S => S): State[S, Unit] = for {
  //   s <- get // Gets the current state and assigns it to `s`.
  //   _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  // } yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))


  def random_int :Rand[Int] =  State (_.nextInt)

  // Exercise 11

  // def state2stream[S,A] (s :State[S,A]) (seed :S) :Stream[A] = ???

  // Exercise 12

  // val random_integers = ???

}






sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Candy {

  // Exercise 13 (CB 6.11)

  // def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}

// vim:cc=80:foldmethod=indent:foldenable