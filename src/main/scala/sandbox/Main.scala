package sandbox

//import cats.instances.string._
//import cats.syntax.semigroup._

object Main extends App {


  //Pre-cats, notes from Typeclass chapter of Essential Scala
  //When looking for type class instances (implicit values), the compiler will look in
  //the local scope AND
  //the companion objects of types involved in the method call
  //**important note = implicits in the local scope take precedence

  final case class Rational(numerator: Int, denominator: Int)

  object Rational {
    implicit val ordering = Ordering.fromLessThan[Rational]((x, y) =>
      (x.numerator.toDouble / x.denominator.toDouble) <
        (y.numerator.toDouble / y.denominator.toDouble)
    )
  }
  object Example {
    def example() = {
      assert(List(Rational(1, 2), Rational(3,4), Rational(1, 3)).sorted ==
        List(Rational(1, 3), Rational(1, 2), Rational(3, 4)))
    }

  }

  //1. When defining a type class instance, if there is a single instance for the type
  //And you can edit the code for the type you are defining the instance for,
  //Then define the type class instance in the companion object of the type

  //2. Local scope takes precedence over instances found in companion objects
  //When defining a type class instance, if there is a single good default instance for the type
  //and you can edit the code for the type you are defining the instance for, then
  //define the type class instance in the companion object of the type - this allows
  //users to override the instance by defining one in the local scope while still providing
  //sensible default behavior

  //3. If there is no good default instance (or multiple defaults) for a type class instance,
  //we should not place type class instances in the companion object, but rather
  //require the user to explicitly import an instance into the local scope
  //one simple way is to place each in its own object that the user can import into the local scope

  //7.2.5.1 Exercise
  final case class Order(units: Int, unitPrice: Double) {
    val totalPrice: Double = units * unitPrice
  }

  object TotalPriceOrdering {
    implicit val ordering = Ordering.fromLessThan[Order]((x, y) =>
    x.totalPrice < y.totalPrice)
  }
  object UnitOrdering{
    implicit val ordering = Ordering.fromLessThan[Order]((x, y) =>
      x.units < y.units)
  }
  object unitPriceOrdering{
    implicit val ordering = Ordering.fromLessThan[Order]((x, y) =>
    x.unitPrice < y.unitPrice)
  }

//Creating Type classes - 4 components
  //1. The actual type class itself - trait with at least one type variable
    //which specify the concrete types the type class instances are defined for
  //2. The type class instances - instance of the trait for each concrete class we want to use
    //and different situation we want to use it in
  //3. Interfaces using implicit params
  //4. Interfacces using enrichment and implicit params

  //Exercise 7.3.4.1

  //Equal is a type class
  trait Equal[A] {
    def equal(x:A, y: A): Boolean
  }

  case class Person2(name: String, email: String)

  //type class instances
  object EqualbyEmail extends Equal[Person]{
    def equal(x:Person, y:Person): Boolean =
      x.email == y.email
  }

  object EqualByNameAndEmail extends Equal[Person]{
    def equal(x:Person, y:Person): Boolean =
      (x.email == y.email) && (x.name == y.name)
  }

  //7.4 Implicit Parameter and Interfaces
trait HtmlWriter[A]{
    def write(in:A): String
  }

object PersonWriter extends HtmlWriter[Person] {
  def write(person: Person) = s"<span>${person.name} &lt;${person.email}&gt;</span>"
}

  //implicit parameter list
  object HtmlUtil {
    def htmlify[A](data: A)(implicit writer: HtmlWriter[A]): String = {
      writer.write(data)
    }
  }


  //println("Hello World")


  //CATS STARTS HERE*****


  //1.1 Anatomy of a Type Class
  //type class is an interface (API) that represents some functionality we want to implement
  //in Cats, a type class is represented by a trait w/ at least one type parameter
  sealed trait Json

  final case class JsObject(get: Map[String, Json]) extends Json

  final case class JsString(get: String) extends Json

  final case class JsNumber(get: Double) extends Json

  final case class JsInt(get: Int) extends Json

  case object JsNull extends Json

  //JsonWriter in a typeclass - Json and its subtypes above provide supporting code
  trait JsonWriter[A] {
    def write(value: A): Json
  }

  final case class Person(name: String, email: String)

  final case class Animal(name: String, habitat: String, colour: String)

  //type class instances provide implementations for the types we care about
  //includes types from the Scala standard library as well as from our domain model
  //create concrete instances of the type class by tagging them with the implicit keyword
  object JsonWriterInstances {
    implicit val stringWriter: JsonWriter[String] =
      new JsonWriter[String] {
        def write(value: String): Json =
          JsString(value)
      }

    implicit val personWriter: JsonWriter[Person] =
      new JsonWriter[Person] {
        def write(value: Person): Json =
          JsObject(Map(
            "name" -> JsString(value.name),
            "email" -> JsString(value.email)
          ))
      }

    implicit val numberWriter: JsonWriter[Double] =
      new JsonWriter[Double] {
        def write(value: Double): Json =
          JsNumber(value)
      }

    implicit val animalWriter: JsonWriter[Animal] =
      new JsonWriter[Animal] {
        def write(value: Animal): Json =
          JsObject(Map(
            "name" -> JsString(value.name),
            "habitat" -> JsString(value.habitat),
            "colour" -> JsString(value.colour)
          ))
      }

    implicit val intWriter: JsonWriter[Int] =
      new JsonWriter[Int] {
        def write(value: Int): Json =
          JsInt(value)
      }
  }

  //a type class interface is any functionality we expose to users
  //interfaces are generic methods & accept instances of the type class as implicit parameters
  // 2 common types: interface objects, interface syntax

  //interface object - simplest way of creating an interface is to place methods in a singleton object
  object Json {
    def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }

  import JsonWriterInstances._

  //the compiler will try to infer which type class instance to use
  val personWriterInterfaceObject = Json.toJson(Person("Michelle", "michelle@example.com"))
  val animalWriterInterfaceObject = Json.toJson(Animal("Ellie", "house", "grey"))

  //println(personWriterInterfaceObject)
  //println(animalWriterInterfaceObject)

  //interface syntax - extension methods extend existing types with interface methods
  object JsonSyntax {

    implicit class JsonWriterOps[A](value: A) {
      def toJson(implicit w: JsonWriter[A]): Json =
        w.write(value)
    }

  }

  import JsonSyntax._

  val personWriterSyntax = Person("Michelle", "michelle@example.com").toJson
  //println(personWriterSyntax)

  //implicitly method - can use implicitly generic type class interface to summon
  //any value from implicit scope
  implicitly[JsonWriter[String]]

  //1.2 Working with implicits
  //1.2.2 Implicit Scope - placing type class instances in a companion object to the type class plays into implicit scope
  //must be tagged with the implicit keyword
  //The compiler searches for candidate type class instances by type in the implicit scope
  //*local or inherited definitions
  //*imported definitions
  //*definitions in the companion object of the type class or the parameter type

  //Appx 4 ways of packaging type class instances
  //*placing them in an object such as JsonWriterInstances as above
  //*placing them in a trait
  //*placing them in the companion object of the type class
  //*placing them in the companion object of the parameter type

  //1.2.3 Recursive Implicit Resolution - 2 ways to define instances
  //*by defining concrete instances as implicit vals of the required type
  //*by defining methods to construct instances from other type class instances

  //in order to handle options, we abstract the code for handling option[A] into a common constructor
  implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
    new JsonWriter[Option[A]] {
      def write(option: Option[A]): Json =
        option match {
          case Some(aValue) => writer.write(aValue)
          case None => JsNull
        }
    }

  val testingOptionWriter = Json.toJson("A string")

  //println(testingOptionWriter)


  //1.3 Exercise - Printable Library
  //1. Define a type class Printable[A] containing a single method format which will accept a value of type A and return a String
  //2. Create an object PrintableInstances containing instances of Printable for String and Int
  //3. Define and object Printable with two generic interface methods:
  //* format which accepts a value of type A and a Printable of the corresponding type
  //* print accepts the same parameters as format and returns unit

  trait Printable[A] {
    def format(value: A): String
  }

  final case class Cat(name: String, age: Int, color: String)

  object PrintableInstances {
    implicit val stringPrintable = new Printable[String] {
      def format(input: String) = input
    }

    implicit val intPrintable = new Printable[Int] {
      def format(input: Int) = input.toString
    }

    implicit val catPrintable = new Printable[Cat] {
      def format(input: Cat) = {
        val name = Printable.format(cat.name)
        val age = Printable.format(cat.age)
        val color = Printable.format(cat.color)
        s"$name is a $age year-old $color cat."
      }
    }
  }

  object Printable {
    def format[A](input: A)(implicit p: Printable[A]): String =
      p.format(input)

    def print[A](input: A)(implicit p: Printable[A]): Unit =
      println(format(input))
  }

  object PrintableSyntax {

    implicit class PrintableOps[A](value: A) {
      def format(implicit p: Printable[A]): String =
        p.format(value)

      def print(implicit p: Printable[A]): Unit =
        println(format(p))
    }

  }

  val cat = Cat("Kitten", 7, "Ginger")
  //cat.print

  //1.4 Meet Cats - Implementing type classes in Cats

  //Exercise Cat Show: re-implement the Cat application from the previous section
  import cats.Show
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.show._

  implicit val catShow = Show.show[Cat] { cat =>
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat."
  }

  println(Cat("Garfield", 38, "ginger and black").show)

  //1.5 Equality, Liberty, Fraternity
  import cats.Eq
  import cats.syntax.eq._
  import java.util.Date
  import cats.instances.option._
  import cats.instances.long._
  import cats.instances.int._

  val eqInt = Eq[Int]
  //println(eqInt.eqv(123, 124))
  //println(123 === 123)
  //println((Some(1): Option[Int]) === (None: Option[Int]))

  implicit val dateEq: Eq[Date] =
    Eq.instance[Date] { (date1, date2) =>
      date1.getTime === date2.getTime
    }

  val x = new Date()
  val y = new Date()

  //Exercise: Equality, Liberty, and Felinity
  implicit val catEq: Eq[Cat] =
    Eq.instance[Cat] {(cat1, cat2) =>
      (cat1.name === cat2.name) &&
        (cat1.age === cat2.age) &&
        (cat1.color === cat2.color)
    }

  val cat1 = Cat("Garfield", 38, "orange and black")
  val cat2 = Cat("Heathcliff", 33, "orange and black")
  val optionCat1 = Option(cat1)
  val optionCat2 = Option.empty[Cat]

  println(cat1 === cat2)
  println(optionCat1 === optionCat2)

}


//1.6 Controlling Instance Selection
//* how do we choose between type classes when there are many available?
//* when we define type classes we can add variance annotations to the type parameter
//* variance relates to subtypes
//COVARIANCE is denoted by + (prefers the more specific type)
  //allows us to sub collections of one type for another in our code
  //useful for modelling collections, like List and Option
  //F[B] is a subtype of F[A] if B is a subtype of A
  //(for example, allows us to sub List[Circle] for List[Shape]) if Circle is a subtype of shape
//CONTRAVARIANCE is denoted by - (supertype instance used)
  //F[B] is a subtype of F[A] if A is a subtype of B
  //useful for modelling that represents processes
  //for example JsonWriter[Shape] is a subtype of JsonWriter[Circle] bc Circle is a subtype of Shape
//INVARIANCE means that types F[A] and F[B] and never subtypes of one another
  //default semantics for Scala type constructors
//When the compiler searches for an implicit, it looks for one matching the type of subtype -
//This allows us to use variance annotations to control type class instance selection



