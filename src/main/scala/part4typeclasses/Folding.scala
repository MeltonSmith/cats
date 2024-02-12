package part4typeclasses

import cats.Eval
import cats.kernel.Monoid

object Folding {

  //TODO - implement all in terms of fold left
  object ListExercises{
    def map[A, B](list: List[A])(f: A => B): List[B] = {
      list.foldLeft(List.empty[B])((a, b) => a :+ f(b))
    }

    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = {
      list.foldLeft(List.empty[B])((a, b) => a ++ f(b))
    }

    def filter[A](list: List[A])(predicate: A => Boolean): List[A] = {
      list.foldLeft(List.empty[A])((a, b) => {
        if (predicate(b))
          a :+ b
        else
          a
      })
    }

    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A = {
      list.foldLeft(monoid.empty)((a, b) => monoid.combine(a, b))
    }
  }


  import cats.Foldable
  import cats.instances.list._ //implicit foldable

  Foldable[List].foldLeft(List(1,2,3), 0)(_ + _)

  import cats.instances.option._ //implicit Foldable[Option]
  Foldable[Option].foldLeft(Option(2), 30)(_ + _) //Some(32)

  //fold right stack safe because of the Eval
  val sumRight: Eval[Int] = Foldable[List].foldRight(List(123), Eval.now(0)) { (num, eval) =>
    eval.map(_ + num)
  }
  import cats.instances.int._
  import cats.instances.string._

  val anotherSum: Int = Foldable[List].combineAll(List(1,2,3))
  val mappedConcat: String = Foldable[List].foldMap(List(1,2,3))(_.toString) // implicit Monoid[String]


  import cats.instances.vector._
  //nesting
  val intNested = List(Vector(1,2,3), Vector(4,5,6))
  (Foldable[List] compose Foldable[Vector]).combineAll(intNested) //implicit Monoid[Int]

  //extension methods
  import cats.syntax.foldable._
  val sum3 = List(1,2,3).combineAll //req Foldable[List], Monoid[Int]
  val mappedConcat2 = List(1,2,3).foldMap(_.toString)


  def main(args: Array[String]): Unit = {


  }

}
