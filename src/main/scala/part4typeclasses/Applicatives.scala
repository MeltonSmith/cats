package part4typeclasses

object Applicatives {

  // Applicatives = Functors + the pure method
  import cats.Applicative
  import cats.instances.list._
  val listApplicative = Applicative[List]

  val aList = listApplicative.pure(2) //List(2)


  import cats.instances.option._ //implicit Applicative[Option]
  val optionApplicative = Applicative[Option]
  val anOption: Option[Int] = optionApplicative.pure(2) // Some(2)

  //pure extension method
  import cats.syntax.applicative._
  val aSweetList = 2.pure[List] //List(2)
  val aSweetOption = 2.pure[Option] //Some(2)

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue: ErrorsOr[Int] = Validated.valid(43) // "pure" method from applicative
  val aModifiedValidated: ErrorsOr[Int] = aValidValue.map(_ + 1) // map from functors


  def validatedApplicative: Applicative[ErrorsOr] = Applicative[ErrorsOr]

  //TODO 1: thought experiment
  def ap[W[_], B, T](wf: W[B => T])(wb: W[B]): W[T] = ???

  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(implicit applicative: Applicative[W]) : W[(A,B)] = {
//    ap(map(fa)(a => (b: B) => (a, b)))(fb)
    val functionWrapper: W[B => (A, B)] = applicative.map(wa)(a => (b: B) => (a, b))


    applicative.ap(functionWrapper)(wb)
  }

  //Applicatives have this ap[W[_], B, T](wf: W[B => T])(wb: W[B]): W[T]
  //Applicatives can implement product from Semigroupal
  // => Applicatives extend Semigroupals

  //Monads extends Applicatives
  //Applicatives extend Functors
  def main(args: Array[String]): Unit = {

  }
}
