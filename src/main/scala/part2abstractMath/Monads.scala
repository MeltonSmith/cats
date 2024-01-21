package part2abstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, ExecutionContextExecutorService, Future}

object Monads {

  implicit val service: ExecutionContextExecutorService = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val numberOption = Option(2)
  val charOption = Option('d')

  //TODO 1.2 combination number char

  val charFuture = Future("s")
  val intFuture = Future(4)


  private val maybeTuple: Option[(Int, Char)] = for {
    num <- numberOption
    char <- charOption
  } yield (num, char)

  /*
    Pattern
    -wrap  a value into a M value
    - the flatMap mechanism
   */

  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]

    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  }

  import cats.Monad
  import cats.instances.option._



  val optionMonad = Monad[Option]
  val anOption = optionMonad.pure(4) //Option(4) == Some(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._

  val listMonad = Monad[List]
  val aList = listMonad.pure(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1)) //List (3,4)

  //TODO 2: use a Monad[Future]
  import cats.instances.future._

  val futMonad = Monad[Future]
  val aFuture = futMonad.pure("YES")
  val aTransformedFuture = futMonad.flatMap(aFuture)(x => Future(x + " after")) //a future that will end up with Success(87)

  //generalize
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A,B)] = {
    monad.flatMap(ma)(a => monad.map(mb)(b => (a,b)))
  }

  def main(args: Array[String]): Unit = {
    println(getPairs(numberOption, charOption))
    getPairs(charFuture, intFuture).foreach(println)

    service.shutdown()
  }
}
