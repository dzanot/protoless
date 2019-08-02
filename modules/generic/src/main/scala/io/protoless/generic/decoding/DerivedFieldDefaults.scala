package io.protoless.generic.decoding

import io.protoless.fields.FieldDefault
import shapeless.{::, Generic, HList, HNil}

trait DerivedFieldDefaults {
  implicit def fieldDefaultHNil: FieldDefault[HNil] = new FieldDefault[HNil] {
    override val default: HNil = HNil
  }

  implicit def fieldDefaultHList[H, T <: HList](implicit
    hDefault: FieldDefault[H],
    tDefault: FieldDefault[T]
  ): FieldDefault[H :: T] = new FieldDefault[H :: T] {
    override val default: H :: T = hDefault.default :: tDefault.default
  }

  implicit def fieldDefaultGeneric[A,  R <: HList](implicit
   gen: Generic.Aux[A, R],
   genDefault: FieldDefault[R],
  ): FieldDefault[A] = new FieldDefault[A] {
    override val default: A = gen.from(genDefault.default)
  }
}

