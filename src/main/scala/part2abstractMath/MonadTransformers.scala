package part2abstractMath

import java.util.concurrent.{Executors, ForkJoinPool}
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {

  def sumAllOptions(values: List[Option[Int]]): Int = ???

  //option transformer
  import cats.data.OptionT
  import cats.instances.future._
  import cats.instances.list._ //fetch an implicit OptionT[List]

  val listOfIntOptions : OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions : OptionT[List, Char] = OptionT(List(Option('b'), Option.empty[Char]))
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    number <- listOfIntOptions
  } yield (number, char)

  // either transformer
  import cats.data.EitherT
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

//  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("something wrong"), Right(43), Right(42)))
//  val futureOfEithers: EitherT[Future, String, Int] = EitherT(Future[Either[String, Int]](Right(45)))
//  val futureOfEithers = EitherT.right(Right(45)) // wrap over Future(Right(45))

  //TODO exercise

  val bandwidths = Map (
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170,
    "server4.rockthejvm.com" -> 50
  )

  type AsyncResponse[T] = EitherT[Future, String, T]

  def getBandwidth(serverName: String): AsyncResponse[Int] = bandwidths.get(serverName) match {
    case None => EitherT.left(Future(s"Server $serverName unreachable"))
    case Some(b) => EitherT.right(Future(b))
  }


  //TODO 1
  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = {
    for {
      value <- getBandwidth(s1)
      value2 <- getBandwidth(s2)
    } yield value + value2 > 250
    //Future[Either[String, Boolean]]
  }

  //TODO 2
  def generateTrafficSpikeReport(s1:String, s2: String): AsyncResponse[String] = {
    canWithstandSurge(s1, s2).transform {
      case Left(err) => Left(s"Servers $s1 and $s2 Can't withstand surge due to $err")
      case Right(false) => Left(s"Servers $s1 and $s2 can't withstand surge due to low capacity")
      case Right(true) => Right(s"Servers $s1 and $s2 can withstand surge")
    }
    // ^^^
    // Future[Either[String, Boolean]] --> Future[Either[String, String]]
  }

  def main(args: Array[String]): Unit = {
//    println(listOfTuples.value)

    val resultFuture = generateTrafficSpikeReport("server5.rockthejvm.com", "server3.rockthejvm.com").value
    resultFuture.foreach(println)
  }

}
