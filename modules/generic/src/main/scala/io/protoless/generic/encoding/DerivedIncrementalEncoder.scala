package io.protoless.generic.encoding

import com.google.protobuf.CodedOutputStream
import io.protoless.messages.encoders.IncrementalEncoder
import shapeless.{Generic, HList, Nat}

abstract class DerivedIncrementalEncoder[A, N <: Nat] extends IncrementalEncoder[A, N]

object DerivedIncrementalEncoder extends IncrementalEncoderInstances
