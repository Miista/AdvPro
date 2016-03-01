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

  def takeWhile1 (p: A => Boolean): Stream[A] =
    foldRight[Stream[A]] (Empty) ((a,b) => cons[A](a, b))

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
    this match {
      case Cons(h,t) if p(h()) => cons(h(), t().filter(p))
      case Cons(_,t) => t().filter(p)
      case Empty => empty[A]
    }

  def append[B >: A] (that: => Stream[B]): Stream[B] =
    foldRight[Stream[B]] (that) (cons (_,_))

  // Exercise 17
  def flatMap[B >: A] (f: A => Stream[B]): Stream[B] = {
    foldRight[Stream[B]] (Empty) ((a, b) => b.append(f(a)))
  }

  def map1[B] (f: A => B): Stream[B] =
    foldRight[Stream[B]] (Empty) ((a,t) => cons[B](f(a), t))

  def filter1 (p: A => Boolean): Stream[A] =
    foldRight[Stream[A]] (Empty) ((v,t) => if (p(v)) cons(v, t) else t)

  def append1[B >: A] (that: => Stream[B]): Stream[B] =
    foldRight[Stream[B]] (that) (cons(_,_))

  // Exercise 17
  def flatMap1[B >: A] (f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]] (Empty) ((h,t) => f(h).append(t))


  //def find (p :A => Boolean) :Option[A] = this.filter (p).headOption

  def startsWith[B >: A] (that: => Stream[B]): Boolean =
    that.headOption1() match {
      case None => true
      case Some(v) => headOption1() match {
        case None => true
        case Some(v1) => v == v1 && tail.startsWith(that.tail)
      }
    }

  def map2[B] (f: A => B): Stream[B] =
    unfold (this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }

  def take2 (n: Int): Stream[A] = ???
  def takeWhile2 (p: A => Boolean): Stream[A] = ???
  def zipWith[A,B,C] (s2: Stream[B])
                   (f: (A,B) => C): Stream[C] = ???
  def zipAll[B] (s2: Stream[B]): Stream[(Option[A],Option[B])] = ???
}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: ()=>A, t: ()=>Stream[A]) extends Stream[A]


object Stream {
  def unfold[A, S] (z: S)
                   (f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => Empty
      case Some((a:A,s:S)) => cons[A] (a, unfold[A,S](s) (f))
    }

  def unfold1[A, S] (z: S)
                   (f: S => Option[(A, S)]): Stream[A] =
    ( for {
        x <- f(z)
        xs = cons [A](x._1, unfold1 [A, S](x._2)(f))
      } yield xs
    ).getOrElse(Empty)

  def unfold2[A,S] (z: S)
                   (f: S => Option[(A,S)]): Stream[A] =
    f(z).fold[Stream[A]] (Empty) (p => cons(p._1, unfold(p._2) (f)))

  def fibs: Stream[Int] = {
    def in(p: Int, c: Int): Stream[Int] =
      cons (p, in(c, p+c))
    in(0,1)
  }

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

  def to (n: Int): Stream[Int] = {
    def go(in: Int, l: Int): Stream[Int] =
      if (in > l) Empty
      else cons[Int](in, go(in+1, l))
    go(1, n)
  }
//    if (n <= 0) Empty else cons[Int](n, to(n-1))

  def from (n: Int): Stream[Int] = cons[Int](n, from(n+1))
}

// vim:tw=0:cc=80:nowrap