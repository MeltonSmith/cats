package part2abstractMath

import scala.annotation.tailrec

object CustomMonads {

  import cats.Monad

  implicit object OptionMonad extends Monad[Option] {
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    override def pure[A](x: A): Option[A] = Option(x)

    //tairecM does not stack-overflow
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None => None
      case Some(Left(v)) => tailRecM(v)(f)
      case Some(Right(b)) => Some(b)
    }
  }

  //TODO 1: define a monad for the identity type
  type Identity[T] = T
  val aNumber: Identity[Int] = 42

  implicit object IdentityMonad extends Monad[Identity] {
    override def flatMap[A, B](a: Identity[A])(f: A => Identity[B]): Identity[B] = {
      f(a)
    }
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(v) => tailRecM(v)(f)
      case Right(b) => b
    }

    override def pure[A](x: A): Identity[A] = x
  }

  sealed trait Tree[+A]
  final case class Leaf[+A](value: A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  //TODO2: define a monad for this tree

  implicit object TreeMonad extends Monad[Tree] {
    override def flatMap[A, B](a: Tree[A])(f: A => Tree[B]): Tree[B] = {
      a match {
        case Leaf(value) => f(value)
        case Branch(left, right) => Branch(flatMap(left)(f) , flatMap(right)(f))
      }
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(t : Tree[Either[A,B]]): Tree[B] = t match {
        case Leaf(Left(v)) => stackRec(f(v))
        case Leaf(Right(b)) => Leaf(b)
        case Branch(left,right) => Branch(stackRec(left), stackRec(right))
      }

      def tailRec(todo: List[Tree[Either[A,B]]], expanded: Set[Tree[Either[A,B]]], done: List[Tree[B]]): Tree[B] = {
        if (todo.isEmpty) done.head
        else todo.head match {
          case Leaf(Left(v)) => tailRec(f(v) :: todo.tail, expanded, done)
          case Leaf(Right(b)) => tailRec(todo.tail, expanded, Leaf(b) :: done)
          case node @ Branch(left, right) => {
            if (!expanded.contains(node)) {
              tailRec(right :: left :: todo, expanded + node, done)
            } else {
              val newLeft = done.head
              val newRight = done.tail.head
              val newBranch = Branch(newLeft, newRight)
              tailRec(todo.tail, expanded, newBranch :: done.drop(2))
            }
          }
        }
      }

      tailRec(List(f(a)), Set(), List())
    }

    override def pure[A](a: A): Tree[A] = {
      Leaf(a)
    }
  }

  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = Branch(Leaf(10), Leaf(20))
    val changedTree = TreeMonad.flatMap(tree)(v => Branch(Leaf(v+1), Leaf(v + 1)))
    println(changedTree)
  }



}
