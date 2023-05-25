package part2abstractmath

import java.util.concurrent.{Executor, Executors}
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {

  def sumAllOptions(values: List[Option[Int]]): Int = ???

  val listOfNumberOpts: List[Option[Int]] = List(Option(1), Option(2))
  val listOfCharOpts: List[Option[Char]] = List(Option('a'), Option.empty)

  //  val listOfOptTuples = for {
  //    number <- listOfNumberOptions
  //    char <- listOfCharOptions
  //  } yield (number, char)

  // Option transformer

  import cats.data.OptionT
  import cats.instances.list._ // fetch  implicit OptionT[List] // Monad[List]

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2))) // we are wrapping a List of Option inside OptionT
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option.empty))
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    number <- listOfNumberOptions
  } yield (number, char)

  // either transformer

  import cats.data.EitherT
  import cats.instances.future._

  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("Sth Wrong"), Right(43), Right(2)))
  implicit val ec:ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(2))
  val futureOfEithers: EitherT[Future, String, Int] = EitherT.left(Future("Oups"))

  // TODO exercise
  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 200
  )

  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(server:String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT.left(Future("Unreachable server"))
    case Some(b) => EitherT.right(Future(b))
  }

  // TODO 1
  // call getBandwidth twice and combine the result


  def canWithstandSurge(s1:String, s2:String):AsyncResponse[Boolean] = for {
    s1Bandwidth <- getBandwidth(s1)
    s2Bandwidth <- getBandwidth(s2)
  } yield s1Bandwidth + s2Bandwidth > 250

  // TODO 2
  // hint: call canWithstandSurge + transform
  def generateTrafficSpikeReport(s1:String, s2:String):AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform {
      case Left(_) => Left("Oups, at least one of the servers does not have the bandwidth configured!")
      case Right(false) => Left( "Oups, at least one of the servers cannot not handle the surge!")
      case Right(true) => Right("Hooray!")
    }

  def main(args: Array[String]): Unit = {
    println(listOfTuples.value) // cartesian product
    canWithstandSurge( "server2.rockthejvm.com",  "server3.rockthejvm.com").map(b => println(s"The server can withstand the surge: $b"))
    generateTrafficSpikeReport("server2.rockthejvm.com",  "server6.rockthejvm.com").value.foreach(println)
  }
}
