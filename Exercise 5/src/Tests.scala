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

}
