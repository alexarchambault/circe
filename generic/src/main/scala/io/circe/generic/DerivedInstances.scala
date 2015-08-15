package io.circe
package generic

import shapeless.{IsTuple, LowPriority, Strict}
import io.circe.generic.derive._

trait MkJsonProductCodecFor[P] {
  def codecFor: JsonProductCodecFor[P]
}

trait LowPriorityMkJsonProductCodecFor {
  implicit def defaultMkJsonProductCodecFor[P]: MkJsonProductCodecFor[P] =
    new MkJsonProductCodecFor[P] {
      def codecFor =
        new JsonProductCodecFor[P] {
          def codec = JsonProductCodec.obj
        }
    }
}

object MkJsonProductCodecFor extends LowPriorityMkJsonProductCodecFor {
  implicit def tupleMkJsonProductCodecFor[T](implicit isTuple: IsTuple[T]): MkJsonProductCodecFor[T] =
    new MkJsonProductCodecFor[T] {
      def codecFor =
        new JsonProductCodecFor[T] {
          def codec = JsonProductCodec.array
        }
    }
}

trait MkJsonSumCodecFor[S] {
  def codecFor: JsonSumCodecFor[S]
}

object MkJsonSumCodecFor {
  implicit def defaultJsonSumCodecFor[S]: MkJsonSumCodecFor[S] =
    new MkJsonSumCodecFor[S] {
      def codecFor =
        new JsonSumCodecFor[S] {
          def codec = JsonSumCodec.obj
        }
    }
}

trait DefaultProductCodec {
  implicit def mkDefaultProductCodec[P]
   (implicit
     instance: Strict[LowPriority[JsonProductCodecFor[P], MkJsonProductCodecFor[P]]]
   ): JsonProductCodecFor[P] =
    instance.value.value.codecFor
}

trait DefaultSumCodec {
  implicit def mkDefaultSumCodec[S]
   (implicit
     instance: Strict[LowPriority[JsonSumCodecFor[S], MkJsonSumCodecFor[S]]]
   ): JsonSumCodecFor[S] =
    instance.value.value.codecFor
}

trait DerivedInstances {

  implicit def mkDerivedDecoder[T]
   (implicit
     instance: Strict[LowPriority[Decoder[T], MkDecodeJson[T]]]
   ): Decoder[T] =
    instance.value.value.decodeJson

  implicit def mkDerivedEncoder[T]
   (implicit
     instance: Strict[LowPriority[Encoder[T], MkEncodeJson[T]]]
   ): Encoder[T] =
    instance.value.value.encodeJson

}
