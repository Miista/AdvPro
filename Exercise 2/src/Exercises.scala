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

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  // override function application to provide a factory of lists (convenience)

  def apply[A](as: A*): List[A] = // Variadic function
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 2
  def tail[A](as: List[A]): List[A] =
    as match {
      case Nil => Nil
      case Cons(_, tail) => tail
    }


  // Exercise 3
  def setHead[A](as: List[A], newHead: A): List[A] = Cons( newHead, as )

  // Exercise 4
  def drop[A](l: List[A], n: Int): List[A] =
    if (n == 0) l
    else l match {
      case Nil => Nil
      case Cons(head, tail) => drop( tail, n-1 )
    }

  // Exercise 5
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] =
    l match {
      case Nil => Nil
      case Cons(head, tail) => if (f(head)) dropWhile(tail, f) else l
    }


  // Exercise 6
  def init[A](l: List[A]): List[A] =
    l match {
      case Nil => Nil
      case Cons(head, Nil) => Nil // Base case
      case Cons(head, tail) => Cons(head, init(tail))
    }


  // Exercise 7 is in the bottom of the file

  // Exercise 8
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def length[A](as: List[A]): Int = foldRight( as, 0 )( (_, acc) => acc + 1 )

  // Exercise 9
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft[A, B](xs, f(z, x))(f)
  }


  // Exercise 10
  def sum(as: List[Int]): Int = foldLeft(as, 0) (_ + _)

  def product(as: List[Int]): Int = foldLeft(as, 1) (_ * _)

  def length1(as: List[Int]): Int = foldLeft(as, 0) ((acc, _) => acc + 1)

  // Exercise 11
  def reverse[A](as: List[A]): List[A] = foldLeft(as, List[A]())(setHead)

  // Exercise 12
  def foldRight1[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, (b: B) => b) (
      (g: (B) => B, a: A) => {
        (b: B) => {
          g( f( a, b ) )
        }
      }
    )(z)
  }

  //noinspection ScalaUnnecessaryParentheses
  def foldLeft1[A,B](as: List[A], z: B)(f: (B,A)=>B) : B = {
    def synthesizer(value: A, fun: (B)=>B): (B)=>B = b => fun( f( b, value ) )
    /* Example (for clarity)
     * val fn: (B) => B = foldRight( as, (b: B) => b )( synthesizer )
     * fn is now a chain of methods. Following this chain results in the final
     * B that we want.
     * We follow this chain simply by invoking the function and as a chain
     * reaction we will in the end get
     * Since fn is a function from B to B we need to
     */
    (foldRight( as, (b: B) => b )( synthesizer )) (z)
    /* Implementing it using foldRight means that foldRight returns a function of (B)=>B
     * which again returns a function from (B)=>B.
     * It's turtles all the way down.
     */
  }

  // Exercise 13
  def append[A](a1: List[A], a2: List[A]): List[A] = {
    a1 match {
      case Nil => a2
      case Cons(head, tail) => Cons(head, append(tail, a2))
    }
  }

  def concat[A](as: List[List[A]]): List[A] = foldLeft(as, List[A]())((b, a) => append(b, a))

  // Exercise 14
  def map[A, B](a: List[A])(f: A => B): List[B] =
    foldRight( a, List[B]() )( (value, acc) => Cons( f( value ), acc ) )

  // Exercise 15 (no coding)

  // Exercise 16
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldLeft(as, List[A]())((acc, v) => if (f(v)) Cons(v, acc) else acc)

  // Exercise 17
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, List[B]())((v, acc) => append(f(v), acc))

  // Exercise 18
  def filter1[A](l: List[A])(p: A => Boolean): List[A] = flatMap(l)(v => if (p(v)) List(v) else Nil)

  // Exercise 19
  def add (l: List[Int]) (r: List[Int]) : List[Int] =
    l match {
      case Nil => Nil
      case Cons(h1, t1) =>
        r match {
          case Nil => Nil
          case Cons(h2, t2) => Cons(h1 + h2, add(t1)(t2))
        }
    }

  // Exercise 20
  def zipWith[A, B, C](f: (A, B) => C)(l: List[A], r: List[B]): List[C] =
    l match {
      case Nil => Nil
      case Cons(h1, t1) =>
        r match {
          case Nil => Nil
          case Cons(h2, t2) => Cons(f(h1, h2), zipWith(f)(t1, t2))
        }
    }

  // Exercise 21
  def hasSubsequence[A](list: List[A], sub: List[A]): Boolean = {
    def inner(innerList: List[A], innerSub: List[A], originalSub: List[A]): Boolean = innerSub match {
      case Nil => true
      case Cons(h, t) =>
        innerList match {
          case Nil => false
          case Cons(head, tail) =>
            if (head == h) inner(tail, t, originalSub)
            else inner(tail, originalSub, originalSub)
        }
    }
    inner(list, sub, sub)
  }

  // Exercise 22

  // def pascal (n :Int) : List[Int] = ...

  // a test: pascal (4) = Cons(1,Cons(3,Cons(3,Cons(1,Nil))))

}


// Exercise 7

object Exercise7 {

  case class SalaryLine(name: String, amount: Integer)

  // def maximumSalary (salaries: List[SalaryLine]) :Integer = ...

  val test_case = List(SalaryLine("John", 41),
    SalaryLine("Alice", 42),
    SalaryLine("Bob", 40))

}