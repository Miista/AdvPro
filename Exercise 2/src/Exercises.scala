import scala.annotation.tailrec
import scala.math.max

// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
//
// AUTHOR1: Ivan Naumovski (inau@itu.dk)
// AUTHOR2: Søren Palmund (spal@itu.dk)
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
// To run the compiled file do "scala Exercises"
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

// An ADT of Lists

/* Exercise 1
 * Result of match expression is: x + y = 1 + 2 = 3
 */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A] (head: A, tail: List[A]) extends List[A]

object List {

  // override function application to provide a factory of lists (convenience)

  def apply[A] (as: A*): List[A] = // Variadic function
    if (as.isEmpty) Nil
    else Cons (as.head, apply (as.tail: _*))

  // Exercise 2
  def tail[A] (as: List[A]): List[A] =
    as match {
      case Nil => Nil
      case Cons (_, tail) => tail
    }

  /* Choices:
   * 1. Return Nil (as we do)
   * 2. Return a List of length 0
   */

  // Exercise 3
  def setHead[A] (as: List[A], newHead: A): List[A] =
    as match {
      case Nil => Nil
      case Cons (_, tail) => Cons (newHead, tail)
    }

  // Exercise 4
  def drop[A] (l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons (head, tail) => drop (tail, n - 1)
    }

  // Exercise 5
  def dropWhile[A] (l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons (head, tail) => if (f (head)) dropWhile (tail, f)
                                else l
    }


  // Exercise 6
  def init[A] (l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons (_, Nil) => Nil // Base case
      case Cons (head, tail) => Cons (head, init (tail))
    }


  // Exercise 7 is in the bottom of the file

  // Exercise 8
  def foldRight[A, B] (as: List[A], z: B)
                      (f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons (x, xs) => f (x, foldRight (xs, z)(f))
    }

  def length[A] (as: List[A]): Int =
    foldRight (as, 0) ((_, acc) => acc + 1)

  // Exercise 9
  @annotation.tailrec
  def foldLeft[A,B] (as: List[A], z: B)
                    (f: (B,A) => B): B =
    as match {
      case Nil => z
      case Cons (x, xs) => foldLeft (xs, f (z, x))(f)
    }


  // Exercise 10
  def sum (as: List[Int]): Int =
    foldLeft (as, 0)(_ + _)

  def product (as: List[Int]): Int =
    foldLeft (as, 1)(_ * _)

  def length1 (as: List[Int]): Int =
    foldLeft (as, 0)((acc, _) => acc + 1)

  // Exercise 11
  def reverse[A] (as: List[A]): List[A] =
    foldLeft (as, List [A]()) ((b,a) => Cons(a, b))

  // Exercise 12
  def foldRight1[A, B] (as: List[A], z: B)
                       (f: (A, B) => B): B =
    foldLeft (as, (b: B) => b)(
      (g: (B) => B, a: A) => {
        (b: B) => {
          g (f (a, b))
        }
      }
    )(z)

  //noinspection ScalaUnnecessaryParentheses
  def foldLeft1[A, B] (as: List[A], z: B)(f: (B, A) => B): B = {
    def synthesizer (value: A, fun: (B) => B): (B) => B = b => fun (f (b, value))
    /* Example (for clarity)
     * val fn: (B) => B = foldRight( as, (b: B) => b )( synthesizer )
     * fn is now a chain of methods. Following this chain results in the final
     * B that we want.
     * We follow this chain simply by invoking the function and as a chain
     * reaction we will in the end get
     * Since fn is a function from B to B we need to
     */
    (foldRight (as, (b: B) => b)(synthesizer)) (z)
    /* Implementing it using foldRight means that foldRight returns a function of (B)=>B
     * which again returns a function from (B)=>B.
     * It's turtles all the way down.
     */
  }

  // Exercise 13
  def append[A] (a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons (head, tail) => Cons (head, append (tail, a2))
    }
  }

  def concat[A] (as: List[List[A]]): List[A] =
    foldLeft (as, List [A]()) (append)

  // Exercise 14
  def map[A, B] (a: List[A])
                (f: A => B): List[B] =
    foldRight1 (a, List[B]()) ((value, acc) => Cons (f (value), acc))

  /* Identify recursive call: From slide 12 from the first week.
   * Under parametric polymorphism.
   *
   * The call is not in tail position because it is part of a call to Cons.
   * This means that the expression 'a::b' will look like Cons(a,b).
   * So b needs to be evaluated before Cons is called.
   */

  // Exercise 15 (no coding)

  // Exercise 16
  def filter[A] (as: List[A])
                (f: A => Boolean): List[A] =
  {
    def fn (value: A, acc: List[A]) =
      if (f (value)) Cons[A] (value, acc) else acc
    foldRight1 (as, List[A]()) (fn)
  }

// Example of foldLeft vs. foldRight
//  def filter[A] (as: List[A])
//                (f: A => Boolean): List[A] =
//  {
//    def fn (acc: List[A], value: A) =
//      if (f (value)) Cons[A] (value, acc) else acc
//    foldLeft1 (as, List[A]()) (fn)
//  }

  // Exercise 17
  def flatMap[A,B] (as: List[A])
                   (f: A => List[B]): List[B] =
  {
    def fn (value: A, acc: List[B]) =
      append (f (value), acc)
    foldRight (as, List[B]()) (fn)
  }

  // Exercise 18
  def filter1[A] (l: List[A])
                 (p: A => Boolean): List[A] =
    flatMap (l) (v => if (p (v)) List (v) else Nil)

  // Exercise 19
  def add (l: List[Int])
          (r: List[Int]): List[Int] =
    (l, r) match {
      case (Nil, _) | (_, Nil) => Nil // Either is empty
      case (Cons (h1, t1), Cons (h2, t2)) => Cons (h1+h2, add (t1)(t2))
    }

  // Exercise 20
  def zipWith[A,B,C] (f: (A, B) => C)
                       (l: List[A], r: List[B]): List[C] =
    (l, r) match {
      case (Nil, _) | (_, Nil) => Nil // Either is empty
      case (Cons(h1, t1), Cons(h2, t2)) => Cons (f (h1, h2), zipWith (f)(t1, t2))
    }

  // Exercise 21
  def hasSubsequence[A] (list: List[A], sub: List[A]): Boolean = {
    def inner(il: List[A], is: List[A]): Boolean =
      (il, is) match {
        case (_, Nil) => true
        case (Nil, _) => false
        case (Cons(lh,lt), Cons(sh,st)) => (lh == sh && inner(lt, st)) || inner(lt, sub)
      }
    inner(list, sub)
  }

  // Exercise 22

  def pascal (n: Int): List[Int] = {
    def row (i: Int, l: List[Int] = List (1)): List[Int] =
      if (i == 1) l
      else row (i-1, zipWith[Int,Int,Int] (_+_) (Cons(0, l), append (l, List (0))))
    row (n)
  }

  // Haskell version
  //  pas :: Int -> [Int] -> [Int]
  //  pas 0 l = l
  //  pas n list = pas (n-1) $ zipWith (+) l r
  //    where l = 0 : list
  //  r = list ++ [0]

  // a test: pascal (4) = Cons(1,Cons(3,Cons(3,Cons(1,Nil))))

}


// Exercise 7

object Exercise7 extends App {

  case class SalaryLine (name: String, amount: Integer)

  def maximumSalary (salaries: List[SalaryLine]): Int =
    salaries match {
      case Nil => -1
      case Cons (s, tail) => max (s.amount, maximumSalary (tail))
    }

  /**
    * maximumSalary written using foldRight1.
    *
    * @param salaries
    * @return
    */
  def maximumSalary1 (salaries: List[SalaryLine]): Int =
    if (List.length (salaries) == 0) -1
    else List.foldRight1 (List.map (salaries)(_.amount), 0) (max (_, _))

  val test_case = List (SalaryLine ("John", 41),
    SalaryLine ("Alice", 42),
    SalaryLine ("Bob", 40))

}
