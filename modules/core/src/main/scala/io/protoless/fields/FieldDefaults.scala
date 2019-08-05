package io.protoless.fields

import java.util.UUID

import com.google.protobuf.ByteString
import io.protoless.tag._
import shapeless.{Unwrapped, tag}

import scala.annotation.implicitNotFound
import scala.collection.generic.CanBuildFrom


@implicitNotFound("No FieldDefault found for type ${A}.")
trait FieldDefault[A] {
 val default: A
}

final object FieldDefault extends HighPriorityFieldDefaults {
 def apply[A](implicit instance: FieldDefault[A]) : FieldDefault[A] = instance
}

trait HighPriorityFieldDefaults extends LowPriorityFieldDefaults {
 private def defaultOf[A](v: A) : FieldDefault[A] = new FieldDefault[A] {
  override val default: A = v
 }

 implicit final val intDefault: FieldDefault[Int] =
  defaultOf[Int](0)
 implicit final val uintDefault: FieldDefault[Int @@ Unsigned] =
  defaultOf[Int @@ Unsigned](tag[Unsigned][Int](0))
 implicit final val sintDefault: FieldDefault[Int @@ Signed] =
  defaultOf[Int @@ Signed](tag[Signed][Int](0))
 implicit final val fintDefault: FieldDefault[Int @@ Fixed] =
  defaultOf[Int @@ Fixed](tag[Fixed][Int](0))
 implicit final val sfintDefault: FieldDefault[Int @@ Signed with Fixed] =
  defaultOf[Int @@ Signed with Fixed](tag[Signed with Fixed][Int](0))

 implicit final val longDefault =
  defaultOf[Long](0)
 implicit final val ulongDefault =
  defaultOf[Long @@ Unsigned](tag[Unsigned][Long](0))
 implicit final val slongDefault =
  defaultOf[Long @@ Signed](tag[Signed][Long](0))
 implicit final val flongDefault =
  defaultOf[Long @@ Fixed](tag[Fixed][Long](0))
 implicit final val sflongDefault =
  defaultOf[Long @@ Signed with Fixed](tag[Signed with Fixed][Long](0))

 implicit final val booleanDefault =
  defaultOf[Boolean](false)

 implicit final val doubleDefault =
  defaultOf[Double](0.0)

 implicit final val floatDefault =
  defaultOf[Float](0.0f)

 implicit final val stringDefault =
  defaultOf[String]("")

 implicit final val uuidDefault =
  defaultOf[UUID](UUID.nameUUIDFromBytes(Array()))

 implicit final val byteStringDefault =
  defaultOf[ByteString](ByteString.EMPTY)

 implicit final val shortDefault =
  defaultOf[Short](0)

 implicit final val bigIntDefault =
  defaultOf[BigInt](BigInt(0))

 implicit final val bigDecimalDefault =
  defaultOf[BigDecimal](BigDecimal(0))

 implicit final val charDefault =
  defaultOf[Char](0)
}

trait LowPriorityFieldDefaults {
  implicit def valueClassDefault[A <: AnyVal, V](implicit
                                                 unwrapped: Unwrapped.Aux[A, V],
                                                 ev: A <:< AnyVal,
                                                 v: FieldDefault[V]
                                                 ): FieldDefault[A] = new FieldDefault[A] {
   override val default: A = unwrapped.wrap(v.default)
  }

 implicit def traversableDefault[A, C[A] <: Traversable[A]](implicit cbf: CanBuildFrom[Nothing, A, C[A]]): FieldDefault[C[A]] = new FieldDefault[C[A]] {
  override val default: C[A] = cbf.apply().result()
 }

 implicit def optionDefault[A]: FieldDefault[Option[A]] = new FieldDefault[Option[A]] {
  override val default: Option[A] = None
 }
}