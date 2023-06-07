package part3datamanipulation

import cats.Id
import cats.data.WriterT

import java.util.concurrent.{Executor, Executors}
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

object Writers {

  import cats.data.Writer

  // 1. define the writer at the start
  val aWriter: Writer[List[String], Int] = Writer(List("Started sth"), 45)
  // 2. manipulate with pure FP
  val anIncreasedWriter = aWriter.map(_ + 1) // value increases and logs stay identical
  val aLogsWriter = aWriter.mapWritten(_ :+ "Found sth interesting") // value stays the same, logs change
  val aWriterWithBoth = aWriter.bimap(_ :+ "New logs", _ * 10) // both value and logs change
  // !! bimap is different from mapBoth because in mapBoth you have access to both values at the same time => they can influence each other :D
  val aWriterWithBoth2 = aWriter.mapBoth((logs, value) => (logs :+ "new log 2", value + 1000))
  // 3. dump either the logs or the value
  val desiredValue = aWriter.value
  val logs = aWriter.written
  val (l, v) = aWriter.run

  import cats.instances.vector._

  val writerA = Writer(Vector("Logs a1", "Logs a2"), 10)
  val writerB = Writer(Vector("Logs b1"), 40)
  val compositeWriter: WriterT[Id, Vector[String], Int] = for {
    valueA <- writerA
    valueB <- writerB
  } yield valueA + valueB
  // what happens to the logs? They will be combined in the natural way in the presence of a Semigroup => the logs will be concatenated

  // reset the logs

  import cats.instances.list._ // import Monoid[List[Int]]

  val anEmptyWriter = aWriter.reset // clear the logs, keep the value

  //todo1: write a function which prints things with writer
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  /*
  Advantage of writers: we don't have side effects, we have pure code
   */
  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    @tailrec
    def rec(number: Int, writer: Writer[Vector[String], Int]): Writer[Vector[String], Int] =
      if (number <= 0) writer.mapWritten(_ :+ "starting")
      else rec(number - 1, writer.mapWritten(_ :+ number.toString))

    val countAndLogsWriter = Writer[Vector[String], Int](Vector(), n)
    rec(n, countAndLogsWriter).mapWritten(_.reverse)
  }


  //todo 2: rewrite this method using writers
  def naiveSum(n:Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  def naiveSumWithWriter(n:Int): WriterT[Id, List[String], Int] = {
      if(n <= 0) Writer[List[String], Int](List(), 0)
      else {
        for {
          _ <- Writer( List(s"Now at $n"), n)
          lowerSum <- naiveSumWithWriter(n - 1)
          _ <- Writer(List(s"Computed sum(${n - 1}) = $lowerSum"), n)
        } yield lowerSum + n
      }
  }

  def main(args: Array[String]): Unit = {
    println(compositeWriter)
    println(countAndSay(10))
    println(countAndLog(10))

    // ex 2
    implicit val  ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(10))
    Future(naiveSum(3)).foreach(println)
    Future(naiveSum(10)).foreach(println)

    val sumFuture1 = Future(naiveSumWithWriter(10))
    val sumFuture2 = Future(naiveSumWithWriter(8))
    sumFuture1.map(_.written)
    sumFuture2.map(_.written)
  }
}
