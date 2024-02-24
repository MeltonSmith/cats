package part4typeclasses

import cats.{Applicative, Foldable, Functor, Monad, Monoid}

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

  import cats.instances.option._
  def filterAsOption(list: List[Int])(predicate: Int => Boolean): Option[List[Int]] =
    listTraverse[Option, Int, Int](list)(n => Some(n).filter(predicate))
  //TODO 4 what's the result of
  val allTrue = filterAsOption(List(2,4,6))(_ % 2 == 0) //Some(List(2,4,6))
  val someFalse = filterAsOption(List(1,2,3))(_ % 2 == 0) //None

  import cats.data.Validated
  import cats.instances.list._ //Semigroup[List] => Applicative[ErrorsOr]]

  type ErrorsOr[T] = Validated[List[String], T]
  def filterAsValidated(list: List[Int])(predicate: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverse[ErrorsOr, Int, Int](list){n =>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"predicate for $n failed"))
    }

  //TODO 5 what's the result of

  val allTrueValidated = filterAsValidated(List(2, 4, 6))(_ % 2 == 0) //Valid(List(2,4,6))
  val someFalseValidated = filterAsValidated(List(1, 2, 3))(_ % 2 == 0) //Invalid(List("predicate for 1", "predicate for 3"))

  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L]{
    def traverse[F[_]: Applicative, A, B](container:L[A])(func: A => F[B]): F[L[B]]
    def listSequence[F[_]: Applicative, A](container: L[F[A]]): F[L[A]] =
      traverse(container)(identity)

    //TODO 6
//    type Identity[T] = T //create fake wrapper
    import cats.Id
    def map[A,B](wa: L[A])(f: A => B) : L[B] = {
      traverse[Id, A, B](wa)(f)
    }
  }

  import cats.Traverse
  import cats.instances.future._ //Applicative[Future]
  val allBandwidthCats = Traverse[List].traverse(servers)(getBandwidth)

  //extension methods
  import cats.syntax.traverse._ //sequence + traverse
  val allBandwidthsWithCats2 = servers.traverse(getBandwidth)

  def main(args: Array[String]): Unit = {
    println(allPairs)
    println(allTriples)
    println(allTrue)
    println(someFalse)
    println(allTrueValidated)
    println(someFalseValidated)
  }

}
