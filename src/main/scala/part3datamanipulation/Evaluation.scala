package part3datamanipulation

object Evaluation {

  /*
  Cats makes the distinction between:
  - evaluating an expression eagerly (default evaluation in Scala)
  - evaluating lazily and every time you request it
  - evaluating lazily and keeping the value (memoizing)
   */

  import cats.Eval

  val instantEval: Eval[Int] = Eval.now {
    println("Computing now") // evaluated eagerly, before wanting to use it
    10
  }

  val redoEval: Eval[Int] = Eval.always { // take a parameter passed by name
    println("Computing again")
    4234
  }

  val delayedEval = Eval.later { // passed by name param
    println("Computing later")
    1000
  }

  val composedEval = instantEval.flatMap(value1 => delayedEval.map(value2 => value1 + value2)) // instantEval is not evaluated one more time
  val anotherComposedEval = for {
    value1 <- instantEval
    value2 <- delayedEval
  } yield value1 + value2 // identical

  //todo 1:
  val evalEx1 = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  // "remember" the computed value
  val dontRecompute = redoEval.memoize // this will hold the internal value without needing to reeval every time


  val tutorial = Eval.always {
    println("step 1: ...");
    "put the guitar on your lap"
  }
    .map { step1 => println("step 2: "); s"$step1 then put your left hand on the neck" }
    .memoize // remember the value up to this point
    .map { steps1And2 => println("Step 3:"); s"$steps1And2 then with the right hand strike the strings" } // the expression will be evaluated every time

  // todo 2:  implement this method such that defer (Eval.now) does NOT run the side effects
  // you want to delay the eval regardless of how Eval was defined
  def defer[T](expr: => Eval[T]): Eval[T] = Eval.later(()).flatMap(_ => expr)

  // todo 3: rewrite the method with Evals
  def reverseList[T](list: List[T]): List[T] = if (list.isEmpty) list else reverseList(list.tail) :+ list.head

  // this will stackoverflow on large lists
  def reverseEval[T](list: List[T]): Eval[List[T]] = if (list.isEmpty) Eval.now(list) else reverseEval(list.tail).map(_ :+ list.head)

  // make it stack safe: defer() instead of Eval.now

  def main(array: Array[String]): Unit = {
    //    println(instantEval.value)
    //    println(redoEval.value)
    //    println(redoEval.value) // the expression is lazily evaluated every time we use it
    //    println(delayedEval.value)
    //    println(delayedEval.value) // the expression is lazily evaluated only once and its value is stored for later use
    //    println(composedEval.value)
    //    println(composedEval.value) // each expression is evaluated only once
    //
    //    println(anotherComposedEval.value)

    //    println(evalEx1.value)
    //  println(tutorial.value)
    //    println(tutorial.value)
    defer(Eval.now(println("hello"))).value
    defer(Eval.now(println("hello"))).value

    println(reverseEval(List(1, 2, 3)).value)
  }
}
