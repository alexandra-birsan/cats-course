package part1intro

import part1intro.Implicits.Person

object Implicits {

  // implicit classes
  final case class Person(name: String) {
    def greet() = s"Hi, my name is $name!"
  }

  implicit class ImpersonableString(name: String) {
    def greet() = Person(name).greet()
  }

  // this is the explicit way, but we want to attach the greet() method to a String (this is the purpose of implicit classes)
  val impersonableString = new ImpersonableString("ALex")
  impersonableString.greet()

  // implicit way -> extension methods
  "Peter".greet() // the compiler does:  new ImpersonableString("Peter").greet()

  // implicit arguments and values
  def increment(x: Int)(implicit y: Int) = x + y

  implicit val b = 10

  def increment2 = increment(20) // the compiler automatically completes with (10)


  // more complex example
  trait JSONSerializer[T] {
    def toJSON(value: T): String
  }

  implicit val PersonJSONSerializer: JSONSerializer[Person] = (person: Person) => {
    s"{ name: ${person.name}}"
  }

  def listToJson[T](values: List[T])(implicit serializer: JSONSerializer[T]): String =
    values.map(serializer.toJSON).mkString("[", ",", "]")

  // implicit method
  implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = (value: T) => {
    s"{ ${value.productElementName(0)} : ${value.productElement(0)} }"
  }

  case class Cat(name: String)

  // !! the compiler will AUTOMATICALLY create a JSON serializer of Cat and make it available

  def main(args: Array[String]): Unit = {

    println(oneArgCaseClassSerializer[Cat].toJSON(Cat("Arra")))
    println(oneArgCaseClassSerializer[Person].toJSON(Person("Alex")))
    println(listToJson(List(Cat("Arra"), Cat("Hatz John"))))
  }
}
