package part2abstractmath

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._ // import the |+| (combine) extension method
  val numbers = (1 to 1000).toList
  // |+| is always associative (sum from left to right = sum from right to left)
  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // define general API
//  def combineFold[T](list:List[T])(implicit semigroup: Semigroup[T]) = list.foldLeft(0)(_ |+| _)

  //MONOIDS
  import cats.Monoid
  val intMonoid = Monoid[Int] // the compiler will fetch the implicit Monoid[Int] from   import cats.instances.int._
  val combineInts = intMonoid.combine(2,25)
  val zero = intMonoid.empty // the intuitive 0 value for that particular type

  import cats.instances.string._ // bring the implicit Monoid[String] in scope
  val emptyString  = Monoid[String].empty // will return the natural neutral element of the type String
  val combineString = Monoid[String].combine("hello ", "world")

  import cats.instances.option._ // construct an implicit Monoid[Option[Int]]
  val emptyOption  = Monoid[Option[Int]].empty // None
  val combineOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) // Some(2)

  // extension methods for Monoids - |+| from Semigroups
  val combinedOptionFancy = Option(3) |+| Option(5)

  // TODO 1: implement a combineFold
  def combineFold[T](list: List[T])(implicit monoid: Monoid[T]):T = list.foldLeft(monoid.empty)(_ |+| _)

  // TODO 2: combine a list of phonebooks as Maps[String, Int]
  val phoneBooks = List(
    Map(
      "Alice" -> 237,
      "Bob" -> 647
    ),
    Map(
      "Charlie" -> 372,
      "Daniel" -> 889
    ),
    Map(
      "Tina" -> 123
    )
  )

  // we don't need to construct a Monoid ourselves
  import cats.instances.map._
//  implicit val mapStringIntMonoid: Monoid[Map[String, Int]] = new Monoid[Map[String, Int]] {
//    override def empty: Map[String, Int] = Map()
//
//    override def combine(x: Map[String, Int], y: Map[String, Int]): Map[String, Int] = x ++ y
//  }

  //TODO 3: SHOPPING cart aggregation with Monoids
  case class ShoppingCart(items:List[String], total:Double)

 implicit val shoppingCartMonoid:Monoid[ShoppingCart] =  Monoid.instance(ShoppingCart(List(), 0d), (x:ShoppingCart, y:ShoppingCart) => ShoppingCart(x.items ++ y.items, x.total + y.total) )
  def checkout(shoppingCarts:List[ShoppingCart]): ShoppingCart = combineFold(shoppingCarts)


  def main(args: Array[String]):Unit = {
    println(sumLeft)
    println(sumRight)
    println(combineOption)
    println(combineFold(numbers))
    println(combineFold(phoneBooks))
  }

}
