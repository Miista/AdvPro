sealed trait MOption[+A] {
  def map[B](f: A => B): MOption[B] = this match {
    case Some(v) => Some(f(v))
    case _ => None
  }

  def getOrElse[B >: A](default: B): B = this match {
    case Some( v ) => v
    case _ => default
  }

  def flatMap[B](f: A => MOption[B]): MOption[B]
  def orElse[B >: A](ob: MOption[B]): MOption[B]
  def filter(f: A => Boolean): MOption[A]
}

case class Some[+A](get: A) extends MOption[A] {
  override def filter(f: (A) => Boolean): MOption[A] = if (f(get)) this else None

  override def orElse[B >: A](ob: MOption[B]): MOption[B] = this

  override def flatMap[B](f: (A) => MOption[B]): MOption[B] = f(get)
}

case object None extends MOption[Nothing] {
  override def filter(f: (Nothing) => Boolean): MOption[Nothing] = this

  override def orElse[B >: Nothing](ob: MOption[B]): MOption[B] = ob

  override def flatMap[B](f: (Nothing) => MOption[B]): MOption[B] = None
}

object Ops {
  def mean(xs: Seq[Double]): MOption[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def lift[A,B](f: A => B): MOption[A] => MOption[B] = _ map f

  def map2[A,B,C](a: MOption[A], b: MOption[B])(f: (A,B) => C): MOption[C] = a match {
    case None => None
    case Some(v) => b match {
      case None => None
      case Some(bv) => Some(f(v, bv))
    }
  }
}

object Tests {
  def test(): Unit = {
    val s = Array(1.0, 2.0, 3.0)
    assert( Ops.mean(s) == Some(2.0) )
    assert( Some(1).map(_*2) == Some(2) )
    assert( Some(1).filter(_==2) == None )
    assert( Some(1).filter(_==1) == Some(1) )
    assert( None.getOrElse(2) == 2 )
    assert( Some(1).getOrElse(2) == 1 )
  }

  def main(args: Array[String]) {
    test()
  }
}
