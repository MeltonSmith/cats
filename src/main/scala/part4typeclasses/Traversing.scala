package part4typeclasses

import cats.{Applicative, Monad, Monoid}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Traversing {

  implicit val ec : ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(10))

  val servers: List[String] = List("server-ci.rockthejvm.com", "server-staging.rockthejvm.com", "prod.rockthejvm.com")

  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  /*
      - we have a List[String]
      - a func string => Future[Int]
       we want a Future[List[Int]]
       */
  val allBandwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) { (accumulator, hostname) =>
    val bandFuture: Future[Int] = getBandwidth(hostname)
    for {
      accBandwidth <- accumulator
      band <- bandFuture
    } yield accBandwidth :+ band
  }


    val allBandwidthsTraverse: Future[List[Int]] = Future.traverse(servers)(getBandwidth)
    val allBandwidthSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

    //TODO 1
    import cats.syntax.applicative._ //pure
    import cats.syntax.flatMap._ //f
    import cats.syntax.functor._ //map
    import cats.syntax.apply._ //mapN

    def listTraverse[F[_]: Applicative, A, B](list:List[A])(func: A => F[B]): F[List[B]] = {
      list.foldLeft(List.empty[B].pure[F])((wAccumulator,elem)=> {
       val wElement: F[B] =  func(elem)
        (wAccumulator, wElement).mapN(_ :+ _)

//        for {
//          acc <- wAccumulator
//          elem <- wElement
//        } yield acc :+ elem
      })
    }

  //TODO 2
  def listSequence[F[_]: Applicative, A](list: List[F[A]]): F[List[A]] = {
    listTraverse(list)(identity)
//    list.foldLeft(List.empty[A].pure[F])((wAc, elem) => (wAc, elem).mapN(_ :+ _))
  }

  //TODO 3
  import cats.instances.vector._
  val allPairs = listSequence(List(Vector(1,2), Vector(3,4))) //Vector[List[Int]] - all possible 2-tuples cartesian product
  val allTriples = listSequence(List(Vector(1,2), Vector(3,4), Vector(5,6))) //Vector[List[Int]] --all the possible 3-pairs


  def main(args: Array[String]): Unit = {
    println(allPairs)
    println(allTriples)
  }

}
