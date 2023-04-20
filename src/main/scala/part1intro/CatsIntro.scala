package part1intro

object CatsIntro {

  // eq - compare values at compile-type and make the code not compile if the values are of different types
  //  val aComparison = 2 == "s string" // NOT COMPILING WITH CATS 2.9.O

  // part 1

  import cats.Eq

  // part 2 - import TS instances for the types you need
  import cats.instances.int._
  import cats.instances.string._
  import cats.instances.double._

  // part 3 -  use the type class
  val intEquality: Eq[Int] = Eq[Int]
  val aTypeSafeComparison = intEquality.eqv(2, 4)
  //  val anUnsafeComparison = Eq.eqv(2,"Alex")// !! the fundamental aspect is that the code does not compile if the values
  // are of different  types

  // part 4 - use extension methods if applicable

  import cats.syntax.eq._

  val anotherTypeSafeComp = 2 === 3 // false
  val neqComparison = 2 =!= 3 // true
  //    val invalidComparison = 2 === "a string" // DOES NOT COMPILE
  // extension methods are only visible in the presence of the right TC instance

  // part 5 -  extending TC operations to composite types, e.g. lists

  import cats.instances.list._ // in cats 2.9.0 is automatically imported. It brings Eq[List[Int]] into scope
  // val aListComparison = List(1,2) === List(1,"s") // does not compile

  // part 6 - apply/ create a TC instance for a custom type
  case class ToyCar(model: String, price: Double)

  implicit val ToyCarEq: Eq[ToyCar] = (x: ToyCar, y: ToyCar) => x.model === y.model && x.price === y.price

  val compare2ToyCars = ToyCar("Ferrari", 299) === ToyCar("Lamborghini", 300)


}
