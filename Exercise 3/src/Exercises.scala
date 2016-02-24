import java.awt.Point
import scala.math.max

// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1:
// AUTHOR2:
// Group number:
//
// Write names and ITU email addresses of both group members that contributed to
// the solution of the exercise (in alphabetical order by family name).
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file is meant to be compiled as follows:
//
// scalac Exercises.scala
//
// or
//
// fsc Exercises.scala
//
// To run the compiled file do "scala Tests"
//
// To load the file int the REPL start the 'scala' interpreter and issue the
// command ':load Exercises.scala'. Now you can interactively experiment with
// your code.
//
// Continue solving exercises in the order presented in the PDF file. Large
// parts of the file are commented in order to make sure that the exercise
// compiles.  Uncomment and complete fragments as you proceed.  The file shall
// always compile and run after you are done with each exercise (if you do them
// in order).  Please compile and test frequently.

// Exercise  1

/* I created OrderedPoint as a trait instead of a class, so I can mix it into
 * Points (this allows me to use java.awt.Point constructors without
 * reimplementing them). As constructors are not inherited, I would have to
 * reimplement them in my subclass.  This is not a problem if I mix in a trait
 * construction time. */

trait OrderedPoint extends java.awt.Point with scala.math.Ordered[Point] {
  def compare (that :java.awt.Point): Int = {
    if (this.x < that.x || (this.x == that.x && this.y < that.y)) {
      return -1
    }
    if (this.x > that.x || (this.x == that.x && this.y > that.y)) {
      return 1
    }
    0
  }
}

//class OrderedPoint extends java.awt.Point with scala.math.Ordered[Point] {
//  override def compare(that: Point): Int = {
//    if (this.x < that.x || (this.x == that.x && this.y < that.y)) {
//      return -1
//    }
//    if (this.x > that.x || (this.x == that.x && this.y > that.y)) {
//      return 1
//    }
//    0
//  }
//}

// Chapter 3

sealed trait Tree[+A]
case class Leaf[A] (value: A) extends Tree[A]
case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 2 (3.25)
  def size[A] (t: Tree[A]): Int =
    t match {
      case Leaf(v) => 1
      case Branch(l,r) => 1 + size(l) + size(r)
    }

  // Exercise 3 (3.26)
  def maximum (t: Tree[Int]): Int =
    t match {
      case Leaf( v ) => v
      case Branch( l, r ) => maximum( l ) max maximum( r )
    }

  // Exercise 4 (3.27)
  def depth[A] (t: Tree[A]): Int =
    t match {
      case Branch(l,r) => max( depth(l), depth(r) ) + 1
      case Leaf(_) => 0
    }

  // Exercise 5 (3.28)
  def map[A, B] (t: Tree[A])
                (f: A => B): Tree[B] =
    t match {
      case Leaf(v) => Leaf( f(v) )
      case Branch(l,r) => Branch[B]( map(l)(f), map(r)(f) )
    }

  // Exercise 6 (3.29)
  def fold[A, B] (t: Tree[A])
                 (f: (B, B) => B)
                 (g: A => B): B =
    t match {
      case Leaf(v) => g(v)
      case Branch(l,r) => f( fold(l)(f)(g), fold(r)(f)(g) )
    }

  def size1[A] (t: Tree[A]): Int =
    1 + fold[A,Int](t) (_+_) (a => 1)

  def maximum1 (t: Tree[Int]): Int =
    fold(t) (max) (identity)

  def depth1[A] (t: Tree[A]): Int =
    fold[A,Int](t) (max(_, _) + 1) (a => 0)

  def map1[A,B] (t: Tree[A])
                 (f: A => B): Tree[B] =
    fold[A,Tree[B]] (t) (Branch[B]) (v => Leaf(f(v)))
}

sealed trait Option[+A] {
  self: Option[A] => // Explicit self-type

  // Exercise 7 (4.1)
  def map[B] (f: A=>B): Option[B] =
    this match {
      case None => None
      case Some(v) => Some( f(v) )
    }

  // Ignore the arrow in default's type this week
  // (it should work (almost) as if it was not there)

  def getOrElse[B >: A] (default: => B): B =
    this match {
      case None => default
      case Some(v) => v
    }

  def flatMap[B] (f: A => Option[B]): Option[B] =
    this match {
      case None => None
      case Some(v) => f(v)
    }


  // Ignore the arrow in ob's type this week
  def orElse[B >: A] (ob: => Option[B]): Option[B] =
    this match {
      case None => ob
      case Some(_) => this
    }

  def filter (f: A => Boolean): Option[A] =
    this match {
      case Some(v) if f(v) => this
      case _ => None
    }
}

case class Some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]

object ExercisesOption {

  // Remember that mean is implemented in Chapter 4 of the text book

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 8 (4.2)
  def variance (xs: Seq[Double]) : Option[Double] =
    mean(xs).flatMap[Double]((m) => Some( xs.map[Double, Seq[Double]]((x) => math.pow(x - m, 2)).fold[Double](0.0)(_ + _)) )

  // Exercise 9 (4.3)
  /**
    * Takes two [[Option]]al values and combines them using
    * the supplied function. If either of the values are [[None]]
    * the entire expression is None.
    *
    * @param ao The first optional value
    * @param bo The second optional value
    * @param f A function that can combine values of type A and B
    * @return Returns the combined value if both values are not-None
    */
  def map2[A,B,C] (ao: Option[A], bo: Option[B])
                  (f: (A,B) => C): Option[C] =
    for {
      a <- ao
      b <- bo
    } yield f(a,b)

  // Exercise 10 (4.4)
  def sequence[A] (as: List[Option[A]]) : Option[List[A]] =
    as.foldRight[Option[List[A]]] (Some(Nil)) (map2(_, _) (_::_)) // Partial application

  // Exercise 11 (4.5)
  def traverse[A,B] (as: List[A]) (f: A => Option[B]): Option[List[B]] = {
    def mapper(item: A, others: Option[List[B]]) =
      for {
        b <- others
        a <- f(item)
      } yield a :: b
    as.foldRight[Option[List[B]]](Some(Nil)) (mapper)
  }
}


// Test cases for running in the compiled vesion (uncomment as you go, or paste
// them into REPL in the interactive version)

object Tests extends App {

  // Exercise 1
  val p = new Point(0,1) with OrderedPoint
  val q = new Point(0,2) with OrderedPoint
  assert(p < q)

  // Notice how we are using nice infix comparison on java.awt
  // objects that were implemented way before Scala existed :) (And without the
  // library implementing a suitable comparator). We did not have to recompile
  // java.awt.Point


  // Exercise 2
  assert (Tree.size (Branch(Leaf(1), Leaf(2))) == 3)
  // Exercise 3
  assert (Tree.maximum (Branch(Leaf(1), Leaf(2))) == 2)
  // Exercise 4
  val t4 = Branch(Leaf(1), Branch(Branch(Leaf(2),Leaf(3)),Leaf(4)))
  assert (Tree.depth (t4) == 3)
  // Exercise 5
  val t5 = Branch(Leaf("1"), Branch(Branch(Leaf("2"),Leaf("3")),Leaf("4")))
  assert (Tree.map (t4) (_.toString) == t5)

  // Exercise 6
  assert (Tree.size1 (Branch(Leaf(1), Leaf(2))) == 3, "Incorrect size using size1")
  assert (Tree.maximum1 (Branch(Leaf(1), Leaf(2))) == 2)
    assert (Tree.depth1 (t4) == 3, "Incorrect depth using depth1")
  assert (Tree.map1 (t4) (_.toString) == t5)

  // Exercise 7
   assert (Some(1).map (x => x +1) == Some(2), "Option1")
   assert (Some(41).getOrElse(42) == 41, "Option2")
   assert (None.getOrElse(42) == 42, "Option3")
   assert (Some(1).flatMap (x => Some(x+1)) == Some(2), "Option3")
   assert ((None: Option[Int]).flatMap[Int] (x => Some(x+1)) == None, "Option4")
   assert (Some(41).orElse (Some(42)) == Some(41), "Option5")
   assert (None.orElse (Some(42)) == Some(42))
   assert (Some(42).filter(_ == 42) == Some(42))
   assert (Some(41).filter(_ == 42) == None)
   assert ((None: Option[Int]).filter(_ == 42) == None)

  // Exercise 8
   assert (ExercisesOption.variance (List(42,42,42)) == Some(0.0), "variance1")
   assert (ExercisesOption.variance (List()) == None, "variance2")
   assert (ExercisesOption.variance (List(10,20,30)) == Some(200.0), "variance3")


  // Exercise 9
   assert (ExercisesOption.map2 (Some(42),Some(7)) (_ + _) == Some(49))
   assert (ExercisesOption.map2 (Some(42),None) (_ + _) == None)
   assert (ExercisesOption.map2 (None: Option[Int],Some(7)) (_ + _) == None)
   assert (ExercisesOption.map2 (None: Option[Int],None) (_ + _) == None)

  // Exercise 10
   assert (ExercisesOption.sequence (List(Some(1), Some(2), Some(42))) == Some(List(1,2,42)))
   assert (ExercisesOption.sequence (List(None,    Some(2), Some(42))) == None)
   assert (ExercisesOption.sequence (List(Some(1), None,    Some(42))) == None)
   assert (ExercisesOption.sequence (List(Some(1), Some(2), None    )) == None)

  // Exercise 11
//   def f (n: Int) :Option[Int] = if (n%2 == 0) Some(n) else None
//   assert (ExercisesOption.traverse (List(1,2,42)) (Some(_)) == Some(List(1,2,42)))
//   assert (ExercisesOption.traverse (List(1,2,42)) (f) == None)

}