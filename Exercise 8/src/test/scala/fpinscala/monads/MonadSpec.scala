// Advanced Programming 2015
// Andrzej Wasowski, IT University of Copenhagen

// Example solutions for Monad exercises, using scalacheck
// Scalacheck's user guide:
// https://github.com/rickynils/scalacheck/wiki/User-Guide

package fpinscala.monads
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary
import scala.language.higherKinds
import Monad._


object  MonadSpec extends Properties("Monad[F[_]] laws..") {

  // Note: The law is fine, but remember that scalacheck has presently a very
  // weak function generator (only generates constant functions)
  def associative[A,F[_]] (m: Monad[F]) (implicit a: Arbitrary[F[A]]): Prop =
    forAll { (x: F[A], f: A => F[A], g: A => F[A]) =>
      m.flatMap[A,A] (m.flatMap[A,A] (x) (f)) (g) ==
      m.flatMap (x) (a => m.flatMap (f(a)) (g))
    }

  def identity[A, F[_]] (m: Monad[F]) (implicit arbFA: Arbitrary[F[A]],
    arbA: Arbitrary[A]): Prop =
      forAll { (x: F[A], f: A => F[A]) =>
      m.flatMap[A,A] (x) (m.unit[A] (_)) == x } :| "right unit" &&
    forAll { (y :A, f: A => F[A]) =>
      m.flatMap[A,A] (m.unit[A](y)) (f) == f(y) } :| "left unit"

  def monad[A,F[_]] (m :Monad[F]) (implicit arbFA: Arbitrary[F[A]],
    arbA: Arbitrary[A]) :Prop =
    associative[A,F] (m) && identity[A,F] (m)

  // uncomment when you have optionMonad
   property ("of optionMonad") = monad[Int,Option] (optionMonad)

  // Exercise 17

   property ("of listMonad with ints") = monad[Int,List] (listMonad)
   property ("of streamMonad with ints") = monad[Int,Stream] (streamMonad)
   property ("of streamMonad with strings") = monad[String,Stream] (streamMonad)

  // Exercise 19

  import fpinscala.monads._

  def kleisliAssociative[A,B,C,D,F[_]] (m: Monad[F])
                                   (implicit arbA: Arbitrary[A], 
                                            arbFB: Arbitrary[F[B]], 
                                            arbB: Arbitrary[B], 
                                            arbFC: Arbitrary[F[C]], 
                                            arbC: Arbitrary[C], 
                                            arbFD: Arbitrary[F[D]], 
                                            arbD: Arbitrary[D]): Prop = {
    forAll { (x: A, f: A => F[B], g: B => F[C], h: C => F[D]) => {
        m.compose (m.compose (f, g), h)(x) == m.compose (f, m.compose (g, h))(x)
      }
    }
  }

  // def kleisliAssociative[A,B,C,D,F[_]](m: Monad[F])
  //                                     (f: A => F[B], g: B => F[C], h: C => F[D]): Prop = {
  //   m.compose (m.compose (f, g), h) == m.compose (f, m.compose (g, h))
  // }

  def kleisliIdentity[A,B,F[_]] (m: Monad[F])
                                (implicit arbFA: Arbitrary[F[A]], arbA: Arbitrary[A]): Prop = {
    forAll { (x: A, f: A => F[A]) => {
        m.compose (f, m.unit (_:A))(x) == f(x) && m.compose (m.unit (_:A), f)(x) == f(x)
      }
    }
  }

  property ("of listMonad with kleisli identity (Int -> Int)") = kleisliIdentity[Int,Int,List] (listMonad)
  property ("of listMonad with kleisli associative (Int -> Int)") = kleisliAssociative[Int,Int,Int,Int,List] (listMonad)
  property ("of listMonad with kleisli associative (Int -> Double -> Float -> String)") = kleisliAssociative[Int,Double,Float,String,List] (listMonad)
  property ("of streamMonad with kleisli associative (String -> Int -> Double -> Boolean)") = kleisliAssociative[String,Int,Double,Boolean,Stream] (streamMonad)
//  def kleisliIdentity[A,B,C,F[_]] (m: Monad[A], f: B => Monad[C]): Unit = {
//    m.compose (m.unit[A] (_), f) == f && m.compose (f, m.unit[B])
////    m.compose(m.unit[A],f) == f && m.compose(f, m.unit[B]) == f
//  }
//  def kleisliAssociative[A, B, C, D, F[_]] (m: Monad[F[B]])
//                                           (f: A => F[B], g: B => F[C], h: C => F[D]) =
//    m.compose (m.compose (f, g), h) == m.compose (f, m.compose (g, h))
//
//  def kleisliIdentity[A, B, F[_]] (m: Monad[F[B]])(f: A => F[B]) =
//    m.compose (f, m.unit) == f && m.compose (m.unit, f) == f
//   def kleisliMonad[A,B,C,D,F[_]] ...

//  property ("of listMonad with ints") = kleisliIdentity[Int,Int,List] (listMonad)()
//  property ("of streamMonad with ints") = monad[Int,Stream] (streamMonad)
//  property ("of streamMonad with strings") = monad[String,Stream] (streamMonad)

  // property ...
  // property ...
  // property ...
  // property ...
  // property ...
}
