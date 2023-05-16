package part2abstractmath

import scala.util.Try

object Functors {

  val aModifiedList = List(1, 2, 3).map(_ + 1) // List(2,3,4)
  val aModifiedOption = Option(2).map(_ + 1) // Some(3)
  val aModifiedTry = Try(22).map(_ + 1)

  // simplified definition of a functor
  trait MyFunctor[F[_]] { // higher kinded type

    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  // cats Functor

  import cats.Functor
  import cats.instances.list._ // includes Functor[List]

  val listFunctor = Functor[List]
  val incrementedNumbers = listFunctor.map(List(14, 2, 3))(_ + 10)

  import cats.instances.option._ // includes Functor[Option]

  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(0))(_ + 100)

  import cats.instances.try_._

  val anIncrementedTry = Functor[Try].map(Try(22))(_ + 10)

  // generalizing an API
  def do10xList(list: List[Int]):List[Int] = list.map(_ * 10)
  def do10XOption(option: Option[Int]):Option[Int] = option.map(_ * 10)

  // generalize
  def do10X[F[_]](container: F[Int])(implicit functor: Functor[F]):F[Int] = functor.map(container)(_ * 10)

  // todo 1: define a functor for a binary tree
  trait Tree[+T] // generally containers are covariant
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]
  // hint: define an object which extends a Functor[Tree]
  implicit object FunctorTree extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(value) => Leaf(f(value))
      case Branch(value, left, right) => Branch(f(value), map(left)(f),map(right)(f))
    }
  }

  case object Tree {

    // smart constructors :D
    // we need them because the type classes are invariant in cats
    def apply[T](value:T): Tree[T] = Leaf(value)
    def apply[T](value:T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }

  // extension method - map !!
  import cats.syntax.functor._
  val tree: Tree[Int] =  Tree(40, Tree(5, Tree(10),Tree(30)), Tree(20))
  val incrementedTree = tree.map(_ * 10) // we have now access to map() even though the trait we defined does not have it

  // todo 2: create a shorter version of the do10X method by using extension methods
  def shorterDo10X[F[_]:Functor](container: F[Int]): F[Int] = container.map(_ * 10)

  def main(array: Array[String]): Unit = {
    println(do10X(List(1,2)))
    println(Option(3))
    println(Try(3))
    println(do10X(Tree(10)))
    println(do10X(Tree(10, Tree(2), Tree(5))))
    println(shorterDo10X( Tree(10, Tree(2), Tree(5))))
  }

}
