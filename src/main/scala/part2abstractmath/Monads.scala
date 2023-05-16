package part2abstractmath

import java.nio.file.LinkOption
import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.sys.exit

object Monads {

  // lists
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  // todo 1.1 how would you create all combinations of (number, character)
  val numberAndCharCombinations: List[(Int, Char)] = numbersList.flatMap(n => charsList.map((n, _)))
  val numberAndCharCombinationsV2 = for {
    n <- numbersList
    c <- charsList
  } yield (n, c) // identical

  // options
  val numberOption = Some(2)
  val charOption = Some('c')
  // todo 1.2 how would you create all combinations of (number, character)
  val numberAndCharOptionCombined = numberOption.flatMap(n => charOption.map(c => (n, c)))

  //futures
  implicit val ec: ExecutionContext =  ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(10))
  val numberFuture = Future(32) // will be evaluated at some point in the future on some thread
  val charFuture = Future('z')

  val numberAndCharFutureCombined =  numberFuture.flatMap(n => charFuture.map(c => (n, c) ))
  val numberAndCharFutureCombinedFor = for {
    n <- numberFuture
    c <- charFuture
  } yield (n,c)

  /*
  Pattern
   -  wrapping a value into M value
   - the ability to transform the M values into other M values : flatMap

   MONADS
   */
  trait MyMonad[M[_]] {

    def pure[A](value: A): M[A]
    // similar to Functor: the only difference is f: A => M[B]
    def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

    // this is the implementation from Monad, so the only abstract methods Monad has are pure and flatMap
    def map[A, B](ma:M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }

  // cats Monad
  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]
  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4) // Option(4) = Some(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 ==0 ) Some(x+1) else None)

  import cats.instances.list._
  val listMonad = Monad[List]
  val aList = listMonad.pure(1) // takes 1  arg of type A. The pure function is very general // List(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x+1))  // List(4, 5)

  // todo use a monad of a Future
  import cats.instances.future._
  val futureMonad = Monad[Future] // requires an implicit EC
  val aFuture = futureMonad.pure(3)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x * 100))

  // specialized API
  def getPairsList(numbers:List[Int], chars:List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map( c => (n, c)))

  def getPairsOption(number:Option[Int], chars:Option[Char]): Option[(Int, Char)] = number.flatMap(n => chars.map(c => (n, c)))

  def getPairsFuture(number:Future[Int], chars:Future[Char]): Future[(Int, Char)] = number.flatMap(n => chars.map(c => (n, c)))

  // obs: the implementation itself didn't change at all for the above methods
  // instead of duplicating methods, we can generalize
  def getPairs[M[_], A, B](ma: M[A], mb:M[B])(implicit monad: Monad[M]): M[(A,B)] = monad.flatMap(ma)( a => monad.map(mb)(b => (a, b)) )

  // extension methods - weirder imports: pure, flatMap
  import cats.syntax.applicative._ // imports pure => you can decorate a pure value
  // OBS: the pure method is part of the Applicative type class
  val oneOption = 1.pure[Option] // implicit Monad[Option] will be used => Some(1)
  val oneList = 1.pure[List] // List(1)

  import cats.syntax.flatMap._  // flatMap is here
  // as long as you have a Monad[T] in scope you will be able to use flatMap of that type :D
  val oneOptionTransformed = oneOption.flatMap(x => (x+1).pure[Option]) // the import is not highlighted because Option already has flatMap

  // todo: implement the map method in MyMonad
  // Monads are Functors => Monads have access to Functor's extension methods
  import cats.syntax.functor._ //map is here
  val oneOptionMapped = oneOption.map(_ + 2)
  val oneOptionMapped2 = Monad[Option].map(Option(2))(_ + 100)

  // for-comprehensions
  val composedOption = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield (one, 2)

  // todo4: implement a shorter version of getPairs using for-comprehensions
  // because we are not using the monad explicitly, we can use it as a context bound :D
  def getPairsWithForComprehension[M[_]:Monad, A, B](ma: M[A], mb:M[B]): M[(A,B)] = for {
    a <- ma
    b <- mb
  } yield (a, b)


  def main(array: Array[String]): Unit = {
    println(numberAndCharCombinations)
    println(numberAndCharCombinationsV2)
    println(numberAndCharOptionCombined)
    numberAndCharFutureCombined.map(println)
    println(getPairs(List(2), List('z')))
    println(getPairsWithForComprehension(List(2), List('z')))

    exit(0)
  }
}
