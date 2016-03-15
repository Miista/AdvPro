import RNG.Simple
import State.Rand

object Quiz5 extends App {
  def random_list[A] (n: Int)(ra: Rand[A]): Rand[List[A]] =
    State.sequence (List.fill[Rand[A]] (n)(ra))

  println (random_list (5)(State.unit (RNG.int)).run(Simple(42)))

  def random_option[A] (ra: Rand[A]): Rand[Option[A]] = {
    State[RNG, Option[A]](rng => {
      val (v,r2) = ra.run (rng)
      (Some(v), r2)
    })
//    State((rng: RNG) => {
//      val tuple: (A, RNG) = ra.run (rng)
//      val a: A = tuple._1
//      (Some(a), tuple._2)
//    })
  }

  def random_int: Rand[Int] = State(rng => rng.nextInt)

  println (random_list (5)(random_option (random_int)).run (Simple(42)))
}