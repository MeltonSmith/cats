package part2abstractMath

import cats.Eq
import part1intro.CatsIntro.ToyCar

object Semigroups {



  //Semigroups COMBINE elements of the same type
  import cats.Semigroup
  import cats.instances.int._

  val naturalIntSemigroup = Semigroup[Int]
  val intCombination = naturalIntSemigroup.combine(2, 46) //addition

  import cats.instances.string._
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination= naturalStringSemigroup.combine("I love ", "Cats")

  //specific API
  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)
  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)

  //support a new Type
  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] { (expense1, expense2) => {
    Expense(Math.max(expense1.id, expense2.id), expense1.amount + expense2.amount)}
  }

  //general API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

  case class Expense(id: Long, amount: Double)


  import cats.syntax.semigroup._
  val anIntSum = 2 |+| 3

  //TODO implement reductThings2 with the |+|
  def reduceThings2[T: Semigroup](list: List[T]): T = list.reduce(_ |+| _)

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    val numbers = (1 to 10).toList
    println(reduceInts(numbers))

    val strings = List("I'm ", "starting ", "to ", "like ", "semigroups")
    println(reduceStrings(strings))

    //general api
    println(reduceThings(numbers))
    println(reduceThings(strings))

    val expenses = List(Expense(1, 299), Expense(2, 300))
    println(reduceThings(expenses))

    import cats.instances.option._

  }

}
