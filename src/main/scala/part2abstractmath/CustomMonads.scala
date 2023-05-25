package part2abstractmath

import scala.annotation.tailrec

object CustomMonads {

  import cats.Monad

  implicit object OptionMonad extends Monad[Option] {

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    // the tailrecM does not stack-overflow
    // there's a tailrec contract for Monads
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None => None
      case Some(Left(v)) => tailRecM(v)(f) // I keep looping until the value is Right, a desirable value
      case Some(Right(v)) => Some(v)
    }

    override def pure[A](x: A): Option[A] = Option(x)
  }

  // todo1: define a monad for the identity type
  type Identity[T] = T // type alias
  val aNumber: Identity[Int] = 23 // this is both an Int and an Identity[Int]

  implicit object IdentityMonad extends Monad[Identity] {

    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f(fa)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Right(v) => v
      case Left(v) => tailRecM(v)(f)
    }

    override def pure[A](x: A): Identity[A] = x
  }

  // harder example
  sealed trait Tree[+A]

  final case class Leaf[+A](value: A) extends Tree[A]

  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // todo2: define monad for this Tree
  // tailrecM tailed is difficult
  implicit object TreeMonad extends Monad[Tree] {

    override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
      case Leaf(v) => f(v)
      case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(t: Tree[Either[A, B]]): Tree[B] = t match {
        case Leaf(Right(v)) => Leaf(v)
        case Leaf(Left(v)) => tailRecM(v)(f)
        case Branch(l, r) => Branch(stackRec(l), stackRec(r))
      }

      stackRec(f(a))
    }

    override def pure[A](x: A): Tree[A] = Leaf(x)
  }

  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = Branch(Leaf(10), Leaf(20))
    val changedTree = TreeMonad.flatMap(tree)(v => Branch(Leaf(v + 1), Leaf(v + 1)))
    println(changedTree)
  }
}


