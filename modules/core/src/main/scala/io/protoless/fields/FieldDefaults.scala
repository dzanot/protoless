package io.protoless.fields

trait FieldDefault[A] {
 val default: A
}

package object defaults {
 implicit object IntDefault extends FieldDefault[Int] {
  val default = 0
 }

 implicit object LongDefault extends FieldDefault[Long] {
  val default = 0
 }

 implicit object BooleanDefault extends FieldDefault[Boolean] {
  val default = false
 }

 implicit object DoubleDefault extends FieldDefault[Double] {
  val default = 0.0
 }

 implicit object FloatDefault extends FieldDefault[Float] {
  val default = 0.0f
 }

 implicit object StringDefault extends FieldDefault[String] {
  val default = ""
 }

 implicit class ListDefault[A](implicit d: FieldDefault[A]) extends FieldDefault[List[A]]{
  val default = Nil
 }


}
