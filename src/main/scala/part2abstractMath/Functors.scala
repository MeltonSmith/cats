package part2abstractMath

import scala.util.Try

object Functors {

  val aModifiedList = List(1, 2, 3).map(_ + 1) // List(2,3,4)
  val aModifiedOption = Option(2).map(_ + 1) // Some(3)
  val aModifiedTry = Try(42).map(_ + 1) //Success 43


  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.int._
  import cats.instances.list._

  val listFunctor = Functor[List]
  val incrementedNumbers = listFunctor.map(List(1,2,3))(_ + 1)

  import cats.instances.option._

  val optionFunctor: Functor[Option] = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(2))(_ + 1) //Some(3)

  import cats.instances.try_._

  //generalizing an API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)

  def do10X[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)


  //TODO 1: define your own functor for a binary type
  // hint: define an object which extends Functor[Tree]
  trait Tree[+T]
  object Tree {
    // "smart" constructors
    def leaf[T](value:T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, leaf: Tree[T], right: Tree[T]) extends Tree[T]

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(v) => Leaf(f(v))
      case Branch(v, left, right) => Branch(f(v), map(left)(f), map(right)(f))
    }
  }

  def do10XShorter[F[_]: Functor](container: F[Int]): F[Int] = {
    import cats.syntax.functor._
    container.map(_ * 10)
  }

  //extensions method - map
  import cats.syntax.functor._
  val tree: Tree[Int] = Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(20)), Tree.leaf(20))
  val incrementedTree: Tree[Int] = tree.map(_ + 1)

  //TODO 2: write  a shorted do10x method using extension method

  def main(args: Array[String]): Unit = {

//    do10X(List(1,2,3))
//    do10X(Option(2))
//    do10X(Try(3))
    println(do10X(Tree.branch(30, Tree.leaf(10), Tree.leaf(20))))
    println(do10XShorter(Tree.branch(30, Tree.leaf(10), Tree.leaf(20))))
  }
}
