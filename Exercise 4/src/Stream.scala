// Advanced Programming 2015
// Andrzej WÄ…sowski, IT University of Copenhagen
//
// meant to be compiled, for example: fsc Stream.scala

package fpinscala.laziness

import fpinscala.laziness.Stream._

sealed trait Stream[+A] {

  def headOption (): Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def headOption1 (): Option[A] =
    foldRight[Option[A]] (None) ((a,_) => Some(a))

  def tail: Stream[A] =
    this match {
      case Empty => Empty
      case Cons(h, t) => t()
    }

  def foldRight[B] (z: => B)
                   (f: (A, => B) => B): B =
    this match {
      case Empty => z
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note 1. eager; cannot be used to work with infinite streams. So foldRight
  // is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z: => B)
                  (f: (A, => B) => B): B =
    this match {
      case Empty => z
      case Cons(h, t) => t().foldLeft(f(h(), z))(f)
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }

  def exists (p: A => Boolean): Boolean =
    this match {
      case Empty => false
      case Cons(h, t) => p(h()) || t().exists(p)
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }

  def toList: List[A] =
    this match {
      case Cons(h,t) => h() :: t().toList
      case Empty => Nil

    }

  def take (n: Int): Stream[A] =
    if (n <= 0) empty[A]
    else this match {
      case Cons(h,t) => cons[A](h(), t().take(n-1))
      case Empty => empty[A]
    }

  def drop (n: Int): Stream[A] =
    if (n == 0) this
    else tail.drop(n-1)

  def takeWhile (p: A => Boolean): Stream[A] =
    this match {
      case Cons(h,t) if p(h()) => cons(h(), t().takeWhile(p))
      case Cons(_,_) => empty[A]
      case Empty => empty[A]
    }

  def takeWhile1 (p: A => Boolean): Stream[A] = foldRight[Stream[A]] (Empty) ((a,b) => cons[A](a, b))

  def forAll (p: A => Boolean): Boolean =
    this match {
      case Cons(h,t) => p(h()) && t().forAll(p)
      case Empty => true
    }

//  Implement map, filter, append, and flatMap
  def map[B] (f: A => B): Stream[B] =
    this match {
      case Cons(h,t) => cons[B] (f(h()), t().map(f))
      case Empty => empty[B]
    }

  def filter (p: A => Boolean): Stream[A] =
    headOption1() match {
      case None => Empty
      case Some(v) if p(v) => cons(v, tail.filter(p))
      case Some(v) => tail.filter(p)
    }

  def append[B >: A] (that: => Stream[B]): Stream[B] =
    headOption() match {
      case None => that
      case Some(v) => cons(v, tail.append(that))
    }

  // Exercise 17
  def flatMap[B >: A] (f: A => Stream[B]): Stream[B] = {
    foldRight[Stream[B]] (Empty) ((a, b) => b.append(f(a)))
  }

//  Compute a lazy stream of Fibonacci numbers fibs: 0, 1, 1, 2, 3, 5, 8, and so on.
//    It can be done with functions available so far. Test it be translating to List a
//  finite prefix of fibs, or a finite prefix of an infinite suffix.
//
//  def fibs

  //def find (p :A => Boolean) :Option[A] = this.filter (p).headOption

  def startsWith[B >: A] (that: => Stream[B]): Boolean =
    that.headOption1() match {
      case None => true
      case Some(v) => headOption1() match {
        case None => true
        case Some(v1) => v == v1 && tail.startsWith(that.tail)
      }
    }
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]


object Stream {
  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  // Note 1: ":_*" tells Scala to treat a list as multiple params
  // Note 2: pattern matching with :: does not seem to work with Seq, so we
  //         use a generic function API of Seq

  def constant[A] (a: A): Stream[A] =
    cons(a, constant(a))

  def to (n: Int): Stream[Int] = cons[Int](n, to(n-1))

  def from (n: Int): Stream[Int] = cons[Int](n, from(n+1))
}

// vim:tw=0:cc=80:nowrap