package part1intro

import part1intro.TypeClasses.JsonSyntax.JsonSerializable

object TypeClasses {

  case class Person(name: String, age: Int)

  // part 1 - type class definition
  trait JsonSerializer[T] {
    def toJson(value: T): String
  }

  // PART 2 - create implicit type class INSTANCES

  implicit object StringSerializer extends JsonSerializer[String] {
    override def toJson(value: String): String = s"""""$value"""""
  }

  implicit object IntSerializer extends JsonSerializer[Int] {
    override def toJson(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JsonSerializer[Person] {
    override def toJson(value: Person): String = s"""{ "name": "${value.name}", "age": ${value.age} }""".stripMargin
  }

  // part 3 - offer some API
  def convertListToJson[T](list: List[T])(implicit serializer: JsonSerializer[T]): String =
    list.map(serializer.toJson).mkString("[", ",", "]")

  // part 4 -  extend the existing types via EXTENSION METHOD
  object JsonSyntax {

    implicit class JsonSerializable[T](value: T) {
      def toJson(implicit serializer: JsonSerializer[T]): String = serializer.toJson(value)
    }

  }
  def main(args: Array[String]): Unit = {
    val bob = Person("Bob", 40)
    println(convertListToJson(List(Person("Alex", 30), bob)))
    // we  need to import JsonSerializable to use the toJson method
    println(bob.toJson)
  }

}
