package part5alien

class ContravariantFunctors {

  trait Format[T] { self => //contravariant type classes
    def format(value: T): String


    def contramap[A](func: A => T): Format[A] = (value: A) => self.format(func(value))
  }

  def format[A](value: A)(implicit f: Format[A]): String = f.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\"" + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean): String = if (value) "Y" else "N"
  }

  //problem: given Format[MyType], can we have a Format[Option[MyType]]
  implicit def getOptionFormat[T](implicit f: Format[T]): Format[Option[T]] = contramap[Option[T], T](_.get)

  def contramap[A,T](func: A => T)(implicit f: Format[T]): Format[A] = new Format[A] {
    override def format(value: A): String = f.format(func(value))
  }


  /*
    Int format
    fo: Format[Option[Int]] = IntFormat.contramap[Option[Int]](_.get)
    fo2: Format[Option[Option[Int]]] = fo.contramap[Option[Option[Int]]](_.get) //second get

    order = Reverse from the written order
    -second get
    -first get
    -format of int
   */

  import cats.Contravariant
  import cats.Show
  import cats.instances.int._
  val showInts = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  import cats.syntax.contravariant._
  val showOptionsShorter: Show[Option[Int]] =  showInts.contramap(_.getOrElse(0))
  def main(args: Array[String]): Unit = {
    println(format(Option(Option(42))))
  }
}
