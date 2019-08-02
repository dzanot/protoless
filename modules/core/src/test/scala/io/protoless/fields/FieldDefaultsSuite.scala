package io.protoless.fields

import java.util.UUID

import com.google.protobuf.ByteString
import io.protoless.tag._
import io.protoless.tests.ProtolessSuite


class FieldDefaultsSuite extends ProtolessSuite {
  private val noBytes: Array[Byte] = Array[Byte]()
  "Decode empty bytes as default value" - {
    "Int" in {
      check[Int]
    }

    "Long" in {
      check[Long]
    }

    "Float" in {
      check[Float]
    }

    "Double" in {
      check[Double]
    }

    "String" in {
      check[String]
    }

    "Boolean" in {
      check[Boolean]
    }

    "ByteString" in {
      check[ByteString]
    }

    "unsigned int32" in {
      check[Int @@ Unsigned]
    }

    "unsigned int64" in {
      check[Long @@ Unsigned]
    }

    "signed int32" in {
      check[Int @@ Signed]
    }

    "signed int64" in {
      check[Long @@ Signed]
    }

    "fixed int32" in {
      check[Int @@ Fixed]
    }

    "fixed int64" in {
      check[Long @@ Fixed]
    }

    "fixed signed int32" in {
      check[Int @@ Signed with Fixed]
    }

    "fixed signed int64" in {
      check[Long @@ Signed with Fixed]
    }

    "Short" in {
      check[Short]
    }

    "Char" in {
      check[Char]
    }

    "BigDecimal" in {
      check[BigDecimal]
    }

    "BigInt" in {
      check[BigInt]
    }

    "UUID" in {
      check[UUID]
    }

    "Option[Int]" in {
      check[Option[Int]]
    }

    "Option[String]" in {
      check[Option[String]]
    }

    "AnyVal" in {
      check[Budget]
    }
  }

  private def check[T](implicit default: FieldDefault[T], decoder: FieldDecoder[T]) = {
    decoder.decode(noBytes, 1).right.value must ===(default.default)
  }

}
