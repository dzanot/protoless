package io.protoless.messages.encoders


import shapeless.Nat

import io.protoless.messages.Encoder

@annotation.inductive
trait IncrementalEncoder[A, N <: Nat] extends Encoder[A]

/**
  * Utilities for [[IncrementalEncoder]]
  */
final object IncrementalEncoder extends {

  def apply[A, N <: Nat](implicit instance: IncrementalEncoder[A, N]): IncrementalEncoder[A, N] = instance

}
