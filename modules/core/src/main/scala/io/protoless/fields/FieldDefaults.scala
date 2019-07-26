package io.protoless.fields

import java.util.UUID

import com.google.protobuf.ByteString
import io.protoless.tag._
import shapeless.{Unwrapped, tag}

import scala.annotation.implicitNotFound

@implicitNotFound("No FieldDefault found for type ${A}.")
trait FieldDefault[A] {
 val default: A
}

final object FieldDefault {
 def apply[A](implicit instance: FieldDefault[A]) : FieldDefault[A] = instance

 private def defaultOf[A](v: A) : FieldDefault[A] = new FieldDefault[A] {
  override val default: A = v
 }

 implicit final val intDefault = defaultOf[Int](0)
 implicit final val uintDefault = defaultOf[Int @@ Unsigned](tag[Unsigned][Int](0))
 implicit final val sintDefault = defaultOf[Int @@ Signed](tag[Signed][Int](0))
 implicit final val fintDefault = defaultOf[Int @@ Fixed](tag[Fixed][Int](0))
 implicit final val sfintDefault = defaultOf[Int @@ Signed with Fixed](tag[Signed with Fixed][Int](0))

 implicit final val longDefault = defaultOf[Long](0)
 implicit final val ulongDefault = defaultOf[Long @@ Unsigned](tag[Unsigned][Long](0))
 implicit final val slongDefault = defaultOf[Long @@ Signed](tag[Signed][Long](0))
 implicit final val flongDefault = defaultOf[Long @@ Fixed](tag[Fixed][Long](0))
 implicit final val sflongDefault = defaultOf[Long @@ Signed with Fixed](tag[Signed with Fixed][Long](0))

 implicit final val booleanDefault = defaultOf[Boolean](false)
 implicit final val doubleDefault = defaultOf[Double](0.0)
 implicit final val floatDefault = defaultOf[Float](0.0f)
 implicit final val stringDefault = defaultOf[String]("")
 implicit final val uuidDefault = defaultOf[UUID](UUID.nameUUIDFromBytes(Array()))
 implicit final val byteStringDefault = defaultOf[ByteString](ByteString.EMPTY)

 implicit final val shortDefault = defaultOf[Short](0)
 implicit final val bigIntDefault = defaultOf[BigInt](BigInt(0))
 implicit final val bigDecimalDefault = defaultOf[BigDecimal](BigDecimal(0))

 implicit final val charDefault = defaultOf[Char](0)


 implicit def valueClassDefault[A <: AnyVal, V](implicit
                                                ev: A <:< AnyVal,
                                                v: FieldDefault[V],
                                                unwrapped: Unwrapped.Aux[A, V]): FieldDefault[A] = new FieldDefault[A] {
  override val default: A = ev(default).asInstanceOf[A]
 }

 implicit def listDefault[A]: FieldDefault[List[A]] = new FieldDefault[List[A]] {
   override val default: List[A] = Nil
 }

 implicit def optionDefault[A]: FieldDefault[Option[A]] = new FieldDefault[Option[A]] {
  override val default: Option[A] = None
 }

}
