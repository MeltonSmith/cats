package part3datamanipulation

object Evaluation {

  import cats.Eval

//  eager
  val instantEval: Eval[Int] = Eval.now {
    println("Computing now!")
    64345
  }

//   lazily and everytime you request it
  val redoVal: Eval[Int] = Eval.always({
    println("Computing again!")
    433
  })

  //lazily and keeping the value
  val delayedEval = Eval.later({
    println("Computing later!")
    12333
  })

  val composedEvaluation = instantEval.flatMap(value1 => delayedEval.map(
    value2 => value1 + value2
  ))

  val anotherComposedEvaluation = for {
    value1 <- instantEval
    value2 <- delayedEval
  } yield value1 + value2 //identical


  //TODO 1: predict the output
  val evalEx1 = for {
    a <- delayedEval
    b <- redoVal
    c <- instantEval
    d <- redoVal
  } yield a+ b+c+d
  // now
  //if we print now, later, again, again, sum, again, again, sum

  // "remember" a computed value
  val dontRecompute = redoVal.memoize

  val tutorial = Eval.always({
    println("Step 1...")
    "puu the guitar on your lap"
  }).map( {step1 => println("step2"); s"$step1 then put your left hand on the fret"})
    .memoize //remember the value up to this point
    .map({step12 => println("Step3, more complicated"); s"$step12 then with the right hand strike the strings"})

  //TODO 2: implement such that(Eval.now) does not run the side effects
  def defer[T](expr: => Eval[T]): Eval[T] = {
//    Eval.later(expr.value)
    Eval.later(()).flatMap(_ => expr)
  }

  //TODO 3: rewrite the mod with Evals
  def reverseList[T](list: List[T]): List[T] = {
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head
  }
//
  def reverseEval[T](list: List[T]): Eval[List[T]] = {
      if (list.isEmpty) Eval.now(list)
      else Eval.defer(reverseEval(list.tail).map(_ :+ list.head))
  }

  def main(args: Array[String]): Unit = {
      println(reverseEval((1 to 10000).toList).value)
  }

}
