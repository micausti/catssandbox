package sandbox

//import cats.instances.string._
//import cats.syntax.semigroup._

object Main extends App {
  println("Hello World")

//  //type class is an interface (API) that represents some functionality we want to implement
//  //in Cats, a type class is represented by a trait w/ at least one type parameter
//  sealed trait Json
//  final case class JsObject(get:Map[String, Json]) extends Json
//  final case class JsString(get:String) extends Json
//  final case class JsNumber(get:Double) extends Json
//  final case class JsInt(get:Int) extends Json
//  case object JsNull extends Json
//
//  //JsonWriter in a typeclass - Json and its subtypes above provide supporting code
//  trait JsonWriter[A] {
//    def write(value: A): Json
//  }
//
//  final case class Person(name: String, email: String)
//  final case class Animal(name: String, habitat: String, colour: String)
//
//  //type class instances provide implementations for the types we care about
//  //includes types from the Scala standard library as well as from our domain model
//  //create concrete instances of the type class by tagging them with the implicit keyword
//  object JsonWriterInstances {
//    implicit val stringWriter: JsonWriter[String] =
//      new JsonWriter[String] {
//        def write(value: String): Json =
//          JsString(value)
//      }
//
//    implicit val personWriter: JsonWriter[Person] =
//      new JsonWriter[Person] {
//        def write(value: Person): Json =
//          JsObject(Map(
//            "name" -> JsString(value.name),
//            "email"-> JsString(value.email)
//          ))
//      }
//
//    implicit val numberWriter: JsonWriter[Double] =
//      new JsonWriter[Double] {
//        def write(value: Double): Json =
//          JsNumber(value)
//      }
//
//    implicit val animalWriter: JsonWriter[Animal] =
//      new JsonWriter[Animal] {
//        def write(value: Animal): Json =
//          JsObject(Map(
//            "name" -> JsString(value.name),
//            "habitat" -> JsString(value.habitat),
//            "colour" -> JsString(value.colour)
//          ))
//      }
//
//    implicit val intWriter: JsonWriter[Int] =
//      new JsonWriter[Int] {
//        def write(value: Int): Json =
//          JsInt(value)
//      }
//  }
//
//  //a type class interface is any functionality we expose to users
//  //interfaces are generic methods & accept instances of the type class as implicit parameters
//  // 2 common types: interface objects, interface syntax
//
//  //interface object
//  object Json{
//    def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
//      w.write(value)
//  }
//
//  import JsonWriterInstances._
//
//  val jsonResult = Json.toJson(Person("Michelle", "michelle@example.com"))
//  println(jsonResult)
}
