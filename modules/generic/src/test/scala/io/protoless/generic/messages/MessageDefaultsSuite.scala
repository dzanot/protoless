package io.protoless.generic.messages

import io.protoless.generic.auto._
import io.protoless.messages.Decoder
import io.protoless.tests.ProtolessSuite

class MessageDefaultsSuite extends ProtolessSuite {
  case class Foo(s: String)
  case class Bar(f: Foo, i: Int)
  case class Qux(i: Int, s: String, b: Boolean, f: Float, l: List[Int], o: Option[Int], nl: List[Foo])
  val base = Qux(1, "1", true, 1.0f, List(1), Some(1), List(Foo("foo")))
  List(8, 1, 18, 1, 49, 24, 1, 37, 0, 0, -128, 63, 42, 1, 1, 48, 1, 58, 5, 10, 3, 102, 111, 111)
  Array(10, 3, 10, 1, 49, 16, 1)
  "Decode missing fields as default values" - {
    "Int" in {
      check(Array(18, 1, 49, 24, 1, 37, 0, 0, -128, 63, 42, 1, 1, 48, 1, 58, 5, 10, 3, 102, 111, 111), base.copy(i = 0))
    }
    "String" in {
      check(Array(8, 1, 24, 1, 37, 0, 0, -128, 63, 42, 1, 1, 48, 1, 58, 5, 10, 3, 102, 111, 111), base.copy(s = ""))
    }
    "Boolean" in {
      check(Array(8, 1, 18, 1, 49, 37, 0, 0, -128, 63, 42, 1, 1, 48, 1, 58, 5, 10, 3, 102, 111, 111), base.copy(b = false))
    }
    "Float" in {
      check(Array(8, 1, 18, 1, 49, 24, 1, 42, 1, 1, 48, 1, 58, 5, 10, 3, 102, 111, 111), base.copy(f = 0f))
    }
    "List" in {
      check(Array(8, 1, 18, 1, 49, 24, 1, 37, 0, 0, -128, 63, 48, 1, 58, 5, 10, 3, 102, 111, 111), base.copy(l = Nil))
    }
    "Option" in {
      check(Array(8, 1, 18, 1, 49, 24, 1, 37, 0, 0, -128, 63, 42, 1, 1, 58, 5, 10, 3, 102, 111, 111), base.copy(o = None))
    }
    "Nested case class" in {
      check(Array(16, 1), Bar(Foo(""), 1))
    }
    "Nested List" in {
      check(Array(8, 1, 18, 1, 49, 24, 1, 37, 0, 0, -128, 63, 42, 1, 1, 48, 1), base.copy(nl = Nil))
    }
  }

  private def check[A: Decoder](a: Array[Byte], expected: A) = {
    val res = Decoder[A].decode(a) match {
      case Left(l) => fail(l.getMessage)
      case Right(r) => r must ===(expected)
    }
  }
}
