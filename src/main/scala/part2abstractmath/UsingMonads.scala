package part2abstractmath

import scala.util.{Failure, Success, Try}

object UsingMonads {

  import cats.Monad
  import cats.instances.list._

  val monadList = Monad[List] // fetch the implicit Monad[List]
  val aList = monadList.pure(1)
  val anExtendedList = monadList.flatMap(aList)(n => List(n, n + 1))
  // applicable to Option, try, Future, etc

  // Either is also a Monad
  val aManualEither: Either[String, Int] = Right(42)

  type LoadingOr[T] = Either[String, T] // type alias
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._

  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(10) // LoadingOr[Int] = Right(10)
  val aChangedLoading = loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n + 1) else Left("Loading meaning of life..."))

  // imaginary online store
  case class OrderStatus(orderId: Long, orderStatus: String)

  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "Ready to ship!"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] = if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data...")
  else Right("Amsterdam")

  val orderId = 457L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(orderStatus => trackLocation(orderStatus))
  // use extension methods

  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val orderLocationBetter = getOrderStatus(orderId).flatMap(trackLocation) // I'm calling the flatMap on the monadic value
  val orderLocationFor = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  // todo: the service layer API of a web app
  case class Connection(host: String, port: String)

  val config = Map(
    "host" -> "localhost",
    "port" -> "2020"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]

    def issueRequest(connection: Connection, payload: String): M[String]
  }
  // do not change the code

  // todo provide a real implementation of HttpService using Try, Option, Future, Either

  object TryHttpService extends HttpService[Try] {

    override def getConnection(cfg: Map[String, String]): Try[Connection] = (for {
      host <- config.get("host")
      port <- config.get("port")
      connection <- Some(Connection(host, port))
    } yield connection).map(Success(_)).getOrElse(Failure(new Exception("Please specify the host and the port")))

    override def issueRequest(connection: Connection, payload: String): Try[String] = Option.when(payload.length < 20)(Success(s"request $payload has been accepted!")).getOrElse(Failure(new Exception("Invalid payload")))
  }

  //todo implement another HttpService with LoadingOr or ErrorOr
  object AggressiveHttpService extends HttpService[ErrorOr] {

    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      (for {
        host <- config.get("host")
        port <- config.get("port")
        connection <- Some(Connection(host, port))
      } yield connection).map(Right(_)).getOrElse(Left(new Exception("Please specify the host and the port")))

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] = Either.cond(payload.length < 20, s"request $payload has been accepted!", new Exception("Invalid payload"))
  }

  // generalized
  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] =
  //    service.getConnection(config).flatMap(service.issueRequest(_, payload))
  // or we can use for-comprehensions
    for {
      connection <- service.getConnection(config)
      response <- service.issueRequest(connection, payload)
    } yield response // this uses the extension methods from  import cats.syntax.functor._

  def main(args: Array[String]): Unit = {
    val triedConnection = TryHttpService.getConnection(config)
    println(triedConnection)
    println(triedConnection.flatMap(TryHttpService.issueRequest(_, "hey333333333333333333333333333333333")))
    println(triedConnection.flatMap(TryHttpService.issueRequest(_, "hey")))

    val aggressiveConnection = AggressiveHttpService.getConnection(config)
    println(aggressiveConnection)
    println(aggressiveConnection.flatMap(AggressiveHttpService.issueRequest(_, "hey333333333333333333333333333333333")))
    println(aggressiveConnection.flatMap(AggressiveHttpService.issueRequest(_, "hey")))

    println(getResponse(AggressiveHttpService, "hey, it's Alex!"))
    println(getResponse(AggressiveHttpService, "hey, it's Alex!!!!!!!!!!!!!!!!!!!!!!!!!"))

  }
}
