package io.protoless.generic.messages

import io.protoless.generic.auto._
import io.protoless.messages.Decoder
import io.protoless.tests.ProtolessSuite

class MessageDefaultsSuite extends ProtolessSuite {
  case class Qux(i: Int, s: String, b: Boolean, f: Float, l: List[Int], o: Option[Int])
  private val base = Qux(1, "1", true, 1.0f, List(1), Some(1))

  "Decode missing fields as default values" - {
    "Int" in {
      check(Array(18, 1, 49, 24, 1, 37, 0, 0, -128, 63, 42, 1, 1, 48, 1), base.copy(i = 0))
    }
    "String" in {
      check(Array(8, 1, 24, 1, 37, 0, 0, -128, 63, 42, 1, 1, 48, 1), base.copy(s = ""))
    }
    "Boolean" in {
      check(Array(8, 1, 18, 1, 49, 37, 0, 0, -128, 63, 42, 1, 1, 48, 1), base.copy(b = false))
    }
    "Float" in {
      check(Array(8, 1, 18, 1, 49, 24, 1, 42, 1, 1, 48, 1), base.copy(f = 0f))
    }
    "List" in {
      check(Array(8, 1, 18, 1, 49, 24, 1, 37, 0, 0, -128, 63, 48, 1), base.copy(l = Nil))
    }
    "Option" in {
      check(Array(8, 1, 18, 1, 49, 24, 1, 37, 0, 0, -128, 63, 42, 1, 1), base.copy(o = None))
    }
  }

  private def check[A: Decoder](a: Array[Byte], expected: A) = {
    Decoder[A].decode(a).right.value must ===(expected)
  }
}
