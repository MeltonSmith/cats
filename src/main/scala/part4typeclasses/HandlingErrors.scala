package part4typeclasses

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object HandlingErrors {

  trait MyApplicativeError[M[_], E] extends Applicative[M] {
    //pure from applicative
    def raiseError[A](e:E): M[A]
    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A]
    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e => pure(func(e)))
  }


  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E] with Monad[M]{
    def ensure[A](m: M[A])(error: E)(predicate: A => Boolean): M[A]

    def raiseError[A](e: E): M[A]
  }

  import cats.MonadError
  import cats.instances.either._ //implicit MonadError
  type ErrorOr[A] = Either[String, A]
  val monadErrorEither = MonadError[ErrorOr, String]

  val success: ErrorOr[Int] = monadErrorEither.pure(32) // Either[String, Int] == Right(32)
  val failure: ErrorOr[Int] = monadErrorEither.raiseError[Int]("Something wrong") // Either[String, Int] == Left("something wrong)


  //recover
  val handledError: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "Badness" => 44
    case _ => 89
  }

  //recoverWith
  val handledError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "Badness" => monadErrorEither.pure(44) //ErrorOr[Int]
    case _ => Left("Something else") //ErrorOr[Int]
  }

  // "filter"
  val filteredSuccess = monadErrorEither.ensure(success)("Number too small")(_ > 100)

  //Try and future

  import cats.instances.try_._ //implicit MonadError[Try], E = Throwable
  val exception = new RuntimeException("Really bad")
  val pureException: Try[Int] = MonadError[Try, Throwable].raiseError(exception) //Failure(exception)
  import cats.instances.future._
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  MonadError[Future, Throwable].raiseError(exception) // Future which will complete with a Failure(exception)

  //applicatives => ApplicativeError
  import cats.data.Validated
  import cats.instances.list._ // implicit Semigroup[List] => ApplicativeError[ErrorsOr, List[String]]
  type ErrorsOr[T] = Validated[List[String], T]
  import cats.ApplicativeError
  val applicativeErrorVal = ApplicativeError[ErrorsOr, List[String]]

  //pure, raiseError, handleError, handleErrorWith









  //extension methods
  import cats.syntax.applicative._ //pure
  import cats.syntax.applicativeError._ //raiseError, HandleError

  val extendedSuccess: ErrorsOr[Int] = 42.pure[ErrorsOr] //requires the implicit applicativeError[ErrorsOr, List[String]]
  val extendedFailure: ErrorsOr[Int] = List("Badness").raiseError[ErrorsOr, Int]
  val recoverError: ErrorsOr[Int] = extendedFailure.recover {
    case _ => 43
  }

  import cats.syntax.monadError._ // ensure
  val testedSuccess: ErrorOr[Int] = success.ensure("Something bad")(_ > 100)

  def main(args: Array[String]): Unit = {

  }

}
