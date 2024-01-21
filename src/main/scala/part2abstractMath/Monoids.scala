package part2abstractMath

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
//  import cats.syntax.semigroup._ // import |+| extension method
  import cats.syntax.monoid._ // import |+| extension method

  val numbers = (1 to 1000).toList
  //|+| is always associative
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)


  //define a general API

//  def combineFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T = {
//    list.foldLeft()(_ |+| _)
//  }

  // MONOIDS
  import cats.Monoid
  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(23, 999) //1024
  val zero = intMonoid.empty // 0

  import cats.instances.string._
  import cats.instances.map._
  val emptyString = Monoid[String].empty // ""
  val combineString = Monoid[String].combine("I understand ", "monoinds")


  import cats.instances.option._ // construct an implicit Monoid[Option[Int]]
  val emptyOption = Monoid[Option[Int]].empty //None
  val combineOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) //Some 2
  val combineOption2 = Monoid[Option[Int]].combine(Option(3), Option(6)) //Some 2

  //extension method for Monoids - |+|
  import cats.syntax.monoid._
//  val combineOptionFancy = Option(3) |+| Option(7)

  //TODO 1: implement a reduceByFold
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]): T = {
    list.foldLeft(monoid.empty)(_ |+| _)
  }


  //TODO 2: combine a list of phonebooks as Maps[String, Int]
  val phonebooks = List(
    Map(
      "Alice" -> 235,
      "Bob" -> 647
    ),
    Map(
      "Charlie" -> 375,
      "Daniel" -> 889
    ),
    Map(
      "Tina" -> 123
    )
  )

  //TODO 3 - shopping cart and online stores with Monoids
  case class ShoppingCart(items: List[String], total: Double)

  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance[ShoppingCart](ShoppingCart(Nil, 0), (lhs, rhs) => {
    ShoppingCart(lhs.items ++ rhs.items, rhs.total + lhs.total)
  })

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = {
    combineFold(shoppingCarts)
  }



  def main(args: Array[String]): Unit = {
//    println(sumLeft)
//    println(sumRight)

//    println(combineFold(List(1, 2, 3)))
//    println(combineFold(List("I ", "Like ", "Monoids")))
//    println(combineFold(phonebooks))
    println(checkout(List(
      ShoppingCart(List("Abc", "BCA"), 200),
      ShoppingCart(List("DBA", "FGH"), 300),
      ShoppingCart(List("RTY", "HJK"), 400))))

  }

}
