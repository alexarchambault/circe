package io.circe.generic

import io.circe.generic.derive.{MkEncodeJson, MkDecodeJson}
import io.circe.{Encoder, Decoder}
import shapeless.{LowPriority, Strict, IsTuple}

/**
 * Semi-automatic codec derivation.
 *
 * This object provides helpers for creating [[io.circe.Decoder]] and [[io.circe.Encoder]] instances
 * for tuples, case classes, "incomplete" case classes, sealed trait hierarchies, etc.
 *
 * Typical usage will look like the following:
 *
 * {{{
 *   import io.circe._, io.circe.generic.semiauto._, io.circe.generic.semiauto.tuple._
 *
 *   case class Foo(i: Int, p: (String, Double))
 *
 *   object Foo {
 *     implicit val decodeFoo: Decoder[Foo] = deriveFor[Foo].decoder
 *     implicit val encodeFoo: Encoder[Foo] = deriveFor[Foo].encoder
 *   }
 * }}}
 */
object semiauto
  extends SingletonInstances
  with DefaultProductCodec
  with DefaultSumCodec {

  object incomplete extends IncompleteInstances

  object tuple {
    implicit def mkTupleDerivedDecoder[T]
     (implicit
       ev: IsTuple[T],
       instance: Strict[LowPriority[Decoder[T], MkDecodeJson[T]]]
     ): Decoder[T] =
      instance.value.value.decodeJson

    implicit def mkTupleDerivedEncoder[T]
     (implicit
       ev: IsTuple[T],
       instance: Strict[LowPriority[Encoder[T], MkEncodeJson[T]]]
     ): Encoder[T] =
      instance.value.value.encodeJson
  }


  def deriveFor[A]: DerivationHelper[A] = new DerivationHelper[A]

  class DerivationHelper[A] {
    def encoder(implicit
      instance: MkEncodeJson[A]
    ): Encoder[A] =
      instance.encodeJson

    def decoder(implicit
      instance: MkDecodeJson[A]
    ): Decoder[A] =
      instance.decodeJson
  }

}
