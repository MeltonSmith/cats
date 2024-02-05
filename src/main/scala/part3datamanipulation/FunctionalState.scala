package part3datamanipulation

object FunctionalState {

  type MyState[S, A] = S => (S, A)

  import cats.data.State

  val countAndSay: State[Int, String] = State(currentCount => (currentCount + 1, s"Counter $currentCount"))
  val (eleven, counted10) = countAndSay.run(10).value

  //state = "iterative" computations
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10, obtained $a"
  a *= 5
  val secondComputation = s"Added 1 to 10, obtained $a"

  //pure FP with states
  val firstTransformation = State((s: Int) => (s + 1, s"Added 1 to 10, obtained ${s+1}"))
  val secondTransformation = State((s: Int) => (s * 5, s"Multiplied with 5, obtained ${s*5}"))

  val compositeTransformation: State [Int, (String, String)] = firstTransformation.flatMap(firstResult =>
    secondTransformation.map(secondResult =>
      (firstResult, secondResult)
  ))

  val compositeTransformation2 = for {
    firstResult <- firstTransformation
    secondResult <- secondTransformation
  } yield (firstResult, secondResult)

  //TODO: an online store
  case class ShoppingCart(items: List[String] = List(), total: Double = 0d)
  def addToCart(item: String, price: Double): State[ShoppingCart, Double] = { State {
    cart => (ShoppingCart(item :: cart.items, cart.total + price), price + cart.total)
  }

  }

  //TODO 2: pure mental gymnastics
  //return a State data structure then when run will not change the state but will issue the value f(a)
  def inspect[A,B](f: A => B): State[A, B] = {
    State[A,B] {
      a => (a , f(a))
    }
  }

  //return value with no changes
  def get[A]: State[A,A] = {
    State[A,A] {
      a => (a,a)
    }
  }

  //returns a state data structure that, when run, returns Unit and sets the state to that value
  def set[A](value: A): State[A, Unit] = {
    State{
      _ => (value, ())
    }
  }

  //returns a State ds when run will return Unit the sets the state to f(state)
  def modify[A](f: A => A): State[A, Unit] = {
    State {
      a => (f(a), ())
    }
  }


  // methods available
  import cats.data.State._


  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)

  def main(args: Array[String]): Unit = {
//    println(compositeTransformation.run(10).value)
//    val add123: State[ShoppingCart, Double] = addToCart("123", 2d)
//    val addAbc: State[ShoppingCart, Double] = addToCart("ABC", 20d)
//
//    val addCart: State [ShoppingCart, Double] = add123.flatMap(fR => addAbc.map(sR => sR))
//    println(addCart.run(ShoppingCart(List("Some"), 4d)).value)
//    or
    val danielCart: State[ShoppingCart, Double] = for {
      _ <- addToCart("Fender guitar", 500)
      _ <- addToCart("Elixir Strings", 19)
      total <- addToCart("Electric cable", 8)
    } yield total
    println(danielCart.run(ShoppingCart()).value)

  }

}
