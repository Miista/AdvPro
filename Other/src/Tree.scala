sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf( v ) => 1
    case Branch( l, r ) => 1 + size( l ) + size( r )
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf( v ) => v
    case Branch( l, r ) => maximum(l) max maximum(r)
  }
}

object Main extends App {
  assert( Tree.size( Leaf( 1 ) ) == 1 )
  assert( Tree.size( Branch( Leaf( 1 ), Leaf( 2 ) ) ) == 3 )

  assert( Tree.maximum( Leaf( 1 ) ) == 1, "maximum #1 failed" )
  assert( Tree.maximum( Branch( Leaf( 1 ), Leaf( 2 ) ) ) == 2, "maximum #2 failed" )
}