package part2abstractmath

object Semigroups {

  // semigroups COMBINE elements of the same type

  import cats.Semigroup
  import cats.instances.int._

  val naturalIntSemigroup = Semigroup[Int] // fetches the implicit Semigroup[Int] instance
  val intCombination = naturalIntSemigroup.combine(2, 3) // 5 addition

  import cats.instances.string._

  private val naturalStringSemigroup: Semigroup[String] = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("Hello", "Cats") // HelloCats concatenation


  //specific API
  def reduceInts(ints: List[Int]): Int = ints.reduce(naturalIntSemigroup.combine) // REDUCE the elements of a list into a single element

  def reduceStrings(strings: List[String]): String = strings.reduce(naturalStringSemigroup.combine)

  // general API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]) = list.reduce(semigroup.combine)

  // TODO: support a new type
  case class Expense(id: Long, amount: Double)

  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup[Expense] { (e1, e2) => Expense(e1.id + e2.id, e1.amount + e2.amount) }

  // part 4: extension methods from Semigroup
  

  def main(args: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)

    // specific API
    val numbers = (1 to 100).toList // the sum of the numbers from 1 to 100
    println(reduceInts(numbers))
    println(reduceStrings(List("Hello", "Cats", "Scala", "Java")))

    // general API
    println(reduceThings(numbers)) // compiler injects Semigroup[Int]
    println(reduceThings(List("Hello", "Cats", "Scala", "Java"))) // compiler injects Semigroup[String]
    import cats.instances.option._
    // the compiler will provide the implicit Semigroup[Option[Int]] instance (because we already have an implicit Semigroup[Int] instance  in scope)
    // the compiler will provide the implicit Semigroup[Option[String]] instance (because we already have an implicit Semigroup[String] instance  in scope)
    // same for any type with an implicit semigroup instance
    println(reduceThings(List(Option(1), Option(2), Option(3), Option(4), None))) // Some(10)
    val stringOptions = List(Option("Hello"), Option("Cats"), Option("Scala"), Option("Java"), None)
    println(reduceThings(stringOptions)) // Some(HelloCatsScalaJava) - concatenation
    println(reduceThings(List(Expense(1, 100), Expense(2, 200), Expense(3, 300)))) // Expense(6,600.0)
  }

}
