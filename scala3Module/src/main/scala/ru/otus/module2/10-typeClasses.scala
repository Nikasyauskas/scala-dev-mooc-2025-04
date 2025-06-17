package ru.otus.module2

import ru.otus.module2.type_classes.JsValue.{JsNull, JsNumber, JsString}


object type_classes {

  sealed trait JsValue
  object JsValue:
    final case class JsObject(get: Map[String, JsValue]) extends JsValue
    final case class JsString(get: String) extends JsValue
    final case class JsNumber(get: Double) extends JsValue
    case object JsNull extends JsValue



  trait JsonWriter[T]:
    def write(v: T): JsValue

  object JsonWriter {
    def apply[T](implicit ev: JsonWriter[T]): JsonWriter[T] = ev
    def from[T](f: T => JsValue) = new JsonWriter[T] {
      override def write(v: T): JsValue = f(v)
    }
    given JsonWriter[Int] with
      def write(v: Int): JsValue = JsNumber(v.toDouble)
    given JsonWriter[String] with
      def write(v: String): JsString = JsString(v)

    given [T](using ev: JsonWriter[T]): JsonWriter[Option[T]] =
      from[Option[T]] {
        case Some(value) => ev.write(value)
        case None => JsNull
      }
  }


  implicit class JsonSyntax[T](v: T) {
    def toJson(using ev: JsonWriter[T]): JsValue =  ev.write(v)
  }



  def toJson[T](v: T)(using jw: JsonWriter[T]): JsValue = {
    jw.write(v)
  }


  toJson("fdvvbfbv")
  toJson(10)
  toJson(Option(10))
  toJson(Option("dfbgfgnhg"))

  "bghbbgfrbgbngf".toJson
  Option(10).toJson

}
















