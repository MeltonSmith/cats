package part2abstractMath



object UsingMonads {
  import cats.Monad
  import cats.instances.list._

  val monadList = Monad[List]
  val aSimpleList: List[Int] = monadList.pure(2)
  val anExtendedList: List[Int] = monadList.flatMap(aSimpleList)(x => List(x, x + 1))
  //applicable to Option, Try, Future


  val aManualEither: Either[String, Int] = Right(42)

  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._
  val loadingMonad = Monad[LoadingOr]
  val anEither = loadingMonad.pure(45) //LoadingOr[Int] == Right(45)
  val aChangedLoading = loadingMonad.flatMap(anEither)(n => if (n % 2 ==0) Right(n + 1) else Left("Loading meaning of life"))

  //imaginary online store
  case class OrderStatus(orderId: Long, status: String)

  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] =
    Right(OrderStatus(orderId, "Ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data...")
    else Right("Amsterdam, NL")

  val orderId = 457L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(orderStatus => trackLocation(orderStatus))
  //use extension methods
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  val orderLocationBetter= getOrderStatus(orderId).flatMap(orderStatus => trackLocation(orderStatus))
  val orderLocationFor = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  //TODO: the service layer API of a webApp

  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  val httpServiceMonad = Monad[ErrorOr]

  def getResponse[M[_] : Monad](service:HttpService[M], payload: String): M[String] = {
    for {
      conn <- service.getConnection(config)
      response <- service.issueRequest(conn, payload)
    } yield response
  }


  //TODO provider a real implementation Try/Option/Future/Either
  object EitherBasedHttpService extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] = {
      if (cfg.contains("host") && cfg.contains("port")) Right(Connection(cfg("host"), cfg("port")))
      else
        Left(new IllegalStateException("cfg is not correct"))
    }

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] = {
      if (connection.host.isEmpty) Left(new IllegalStateException("Something happened..."))
      else Right(s"Request ${payload} has been accepted")
    }
  }




  def main(args: Array[String]): Unit = {

   println(getResponse(EitherBasedHttpService, "Some Load"))


  }

}
