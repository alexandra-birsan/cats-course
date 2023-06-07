package part3datamanipulation

object Readers {

  /*
   - configuration file => initial data structure
   - a DB layer
   - an HTTP layer
   - a business logic layer
   */
  case class Configuration(dbUsername: String, dbPassword: String, host: String, port: Int, nThreads: Int, emailReplyTo: String)

  case class DbConnection(username: String, pass: String) {
    def getOrderStatus(orderId: Long): String = "dispatched" // select * from the DB table and return the status of the order id

    def getLastOrderId(username: String): Long = 103545 // select max(orderId) from table where username =  username
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = println("Server started") // this would start the actual server
  }

  // bootstrap
  val configuration = Configuration("alex", "secret", "localhost", 4000, 10, "alex@hello.com")

  import cats.data.Reader

  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbUsername))
  val dbConn = dbReader.run(configuration)

  //  Reader[I, O]
  val alexsOrderStatusReader: Reader[Configuration, String] = dbReader.map(dbConn => dbConn.getOrderStatus(55L))
  val alexsOrderStatus: String = alexsOrderStatusReader.run(configuration)

  def getLastOrderStatus(username: String): String = {
    val lastOrderStatusReader = dbReader.map(_.getLastOrderId(username)).flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))
    // identical
    val lastOrderStatusForReader = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      lastOrderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield lastOrderStatus
    //    lastOrderStatusReader.run(configuration)
    lastOrderStatusForReader.run(configuration)
  }

  /*
  Pattern
  1. create the initial data structure
  2. create a reader which specifies how the data structure will be manipulated later
  3. you can map & flatMap the reader to produce derived information
  4. when you need the final piece of info, you call run on the reader with the initial data structure
   */

  // TODO
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) = s"From: $emailReplyTo; to: $address >>> $contents"
  }

  val emailServiceReader = Reader[Configuration, EmailService](conf => EmailService(conf.emailReplyTo))

  def emailUser(username: String, userEmail: String): String = {
    // fetch the status of their last order
    // email them with the Email service: "Your order has the status: (status)"
    val emailReader = for {
      lastUserOrderId <- dbReader.map(_.getLastOrderId(username))
      lastOrderStatus <- dbReader.map(_.getOrderStatus(lastUserOrderId))
      emailService <- emailServiceReader
    } yield emailService.sendEmail(userEmail, s"Your order has the status: $lastOrderStatus")
    emailReader.run(configuration)
  }

  // todo 2: what programming pattern do Readers remind you of?
  // my answer: decorator
  // answer: Dependency injection :D


  def main(array: Array[String]): Unit = {
    println(getLastOrderStatus("alex"))
    println(emailUser("alex", "alex@goddess.com"))
  }

}
