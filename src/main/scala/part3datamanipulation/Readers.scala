package part3datamanipulation

object Readers {

  /*
    - config file => initial data structure
    - a DB layer
    - an HTTP layer
    - a business logic layer
   */

  case class Configuration(dbUserName: String, dbPassword:String, host: String, port: Int, nThreads: Int, emailReplyTo: String)
  case class DbConnection(username:String, password: String){
    def getStatus(orderId: Long): String = "dispatched" // select * from the db table and return the status of the orderId
    def getLastOrderId(userName: String): Long = 1321 //select max(orderId) from table where username = username
  }

  case class HttpService(host: String, port: Int) {
    def start(): Unit = print("server started") // this would start the actual server
  }

  // bootstrap
  val config = Configuration("abc", "password", "localhost", 1234, 8, "some@gmail.com")

  //cats Reader
  import cats.data.Reader
  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUserName, conf.dbPassword))
  val dbConn = dbReader.run(config)

  //Reader[I, O]

  val someonesOrderStatusReader: Reader[Configuration, String] = dbReader.map(dbConn => dbConn.getStatus(55))
  val danielOrderStatus: String = someonesOrderStatusReader.run(config)

  def getLastOrderStatus(userName: String): String = {
    val usersLastOrderId: Reader[Configuration, String] = dbReader.map(_.getLastOrderId(userName))
      .flatMap(lastOrderId => dbReader.map(dbConn => dbConn.getStatus(lastOrderId)))
    usersLastOrderId.run(config)

    val usersOrderFor: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(userName))
      oderStatus <- dbReader.map(_.getStatus(lastOrderId))
    } yield oderStatus

    usersOrderFor.run(config)
  }

  /*
   Pattern
   1. You create the initial data structure
   2. you create a reader which specifies how that data structure will be manipulated later
   3. you can then map and flatmap the reader to produce derived information
   4. when you need the final piece of information, you call run on the reader with the initial data structure
   */

  //TODO 1
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String) = s"Sending email to $address: $contents"
  }


  //TODO : what programming patter do Reader remind you of?
  // Dependency injection!


  def emailUser(userName: String, userEmail: String): String = {
    val emailServiceReader: Reader[Configuration, EmailService] = Reader(config => EmailService(config.emailReplyTo))
    val emailReader: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(userName))
      oderStatus <- dbReader.map(_.getStatus(lastOrderId))
      emailService <- emailServiceReader
    } yield emailService.sendEmail(userEmail,  s"Your last order $lastOrderId has the status: $oderStatus")

    //fetch the status of their last order
    // email them with the Email service: "Your last order has the status: {status}"
    emailReader.run(config)
  }

  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("dsda"))
    println(emailUser("dasddsa", "dsda@gmail.com"))


  }

}
