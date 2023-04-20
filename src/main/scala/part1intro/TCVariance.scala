package part1intro

object TCVariance {

  import cats.Eq
  import cats.instances.int._ // brings into the scope Eq[Int] instance
  import cats.instances.option._ // brings into the scope Eq[Option[Int]] instance
  import cats.syntax.eq._

  val aComparison = Option(2) === Option(42)
  //  val anInvalidComparison = Some(2) === None // we don't have access to Eq[Option.type] instance
  // Eq[Some[Int]] NOT FOUND, EVEN THOUGH Some is a subtype of option
  // => Eq[Option[Int]] is NOT variant

  // variance
  class Animal

  class Cat extends Animal


  // define a covariant type:
  class Cage[+T] // + allows the subtyping to be propagated to the generic type

  val cage: Cage[Animal] = new Cage[Cat] // Cat is a subtype of Animal => Cage[Cat] <: Cage[Animal]

  // contravariant type: propagates the subtyping in the opposite direction to the generic type
  class Vet[-T]

  val vet: Vet[Cat] = new Vet[Animal] // Cat is a subtype of Animal => Vet[Animal] <: Vet[Cat] !!!

  // rule of thumb: "HAS A T" => covariant, " ACTS ON T" => contravariant

  // contravariant TC
  trait SoundMaker[-T]

  implicit object AnimalSoundMaker extends SoundMaker[Animal]

  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("sound")

  makeSound[Animal] // sound
  makeSound[Cat] // sound -
  // rule of thumb: contravariance -> if you don't have the exact type available, the compiler also  tries to find a more generic type

  // has implications for subtypes
  implicit object OptionSoundMaker extends SoundMaker[Option[Int]] // !!! OptionSoundMaker <: SomeSoundMaker

  makeSound[Option[Int]] // sound
  makeSound[Some[Int]] // sound


  class MyWrapperCov[+T]

  class MySubtypeWrapperCov[+T] extends MyWrapperCov[T]

  implicit object MyWrapperCovSoundMaker extends SoundMaker[MyWrapperCov[Int]]

  makeSound[MyWrapperCov[Int]] // sound
  makeSound[MySubtypeWrapperCov[Int]] // sound
  // expl: MySubtypeWrapperCov[Int] <: MyWrapper[Int]
  //       SoundMaker[MySubtype] >: SoundMaker[MyWrapper]
  // contravariance: where we pass the specific type (MyWrapper[Int]), we can also pass the generic type (MySubtypeWrapper[Int])


  // type class pattern

  // covariant TC
  trait AnimalShow[+T] { // contains !!! some animals !!!
    def show: Unit

  }

  // IMPLICIT TC INSTANCES
  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: Unit = println("animal")
  }

  implicit object CatAnimalShow extends AnimalShow[Cat] {
    override def show: Unit = println("cat")
  }

  // API for users to call
  def showAnimal[T](implicit animalShow: AnimalShow[T]): Unit = animalShow.show

  showAnimal[Cat] // cat -- the compiler will import CatAnimalShow in the scope
  //  showAnimal[Animal] // will not compile - ambiguous implicits

  def dummyMethod[T >: Animal](a: T) = println("Hello")

  def main(args: Array[String]): Unit = {
    dummyMethod(new Cat) // ok
    dummyMethod(new Animal) // ok

    dummyMethod(GeneralAnimalShow)
    dummyMethod(CatAnimalShow)
    dummyMethod(new AnimalShow[Animal] {
      override def show: Unit = println("animal")
    })
  }

  // rule 3: you can't have both benefits
  // cats has INVARIANT TYPE CLASSES
  // if you want to check equality of Some and None, then use the smart constructors of the type
  Option(2) === Option.empty[Int] // true
}
