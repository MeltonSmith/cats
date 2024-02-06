package part4typeclasses

import cats.Monad

object Semigroupals {

  trait MySemigroupal[F[_]] {
    def product[A,B](fa: F[A], fb: F[B]) : F[(A,B)]
  }

  import cats.Semigroupal
  import cats.instances.option._ //implicit Semigroupal[Option]
  val optionSemigroupal = Semigroupal[Option]

  val aTupleOption = optionSemigroupal.product(Some(123), Some("a sstring")) // Some((123, "a string))
  val aNoneTupled = optionSemigroupal.product(Some(123), None) // none

  import cats.instances.list._
  val aTupledList = Semigroupal[List].product(List(1,2), List("a", "b"))


  // TODO: implement
  import cats.Monad
  import cats.syntax.functor._
  import cats.syntax.flatMap._

  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A,B)] = {
//    monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))

    for {
      a <- fa
      b <- fb
    } yield (a,b)
  }

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr] //requires the implicit semigroup[List[_]]
  val invalidCombinaton = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "something else went wrong")),
    Validated.invalid(List("This can't be right"))
  )

  //we're losing the errorcks track when using Monad flatmaps
  //the last left is not printed
  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._ //implicit Monad[Either]
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eitherCombination = eitherSemigroupal.product( // in terms of map/flatMap
    Left(List("Something wrong", "something else went wrong")),
    Left(List("This can't be right"))
  )

  //TODO2: define a Semigroupal[List] which does list
  //or new Semigroupal
  object ZipSemigroupal extends Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = {
       fa.zip(fb)
    }
  }


  def main(args: Array[String]): Unit = {
//    println(invalidCombinaton)
//    println(eitherCombination)
    println(ZipSemigroupal.product(List(1,2), List("a", "b")))
  }

}
