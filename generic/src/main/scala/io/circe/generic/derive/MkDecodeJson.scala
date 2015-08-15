package io.circe
package generic
package derive

import cats.data.Xor
import shapeless._
import shapeless.labelled.{ field, FieldType }

trait MkDecodeJson[T] {
  def decodeJson: Decoder[T]
}

trait ProductDecodeJson[T] {
  def apply(productCodec: JsonProductCodec): Decoder[T]
}

trait ADecoder[T] {
  def apply(a: ACursor): Xor[DecodingFailure, T]
}

trait HListProductDecodeJson[L <: HList] {
  def apply(productCodec: JsonProductCodec): ADecoder[L]
  def decoder(productCodec: JsonProductCodec): Decoder[L] =
    Decoder.instance {
      val underlying = apply(productCodec)
      c =>
        underlying(c.acursor)
    }
}

object HListProductDecodeJson {
  def apply[L <: HList](implicit decodeJson: HListProductDecodeJson[L]): HListProductDecodeJson[L] =
    decodeJson

  implicit def hnilDecodeJson: HListProductDecodeJson[HNil] =
    new HListProductDecodeJson[HNil] {
      def apply(productCodec: JsonProductCodec) =
        new ADecoder[HNil] {
          def apply(c: ACursor) =
            productCodec
              .decodeEmpty(c)
              .map(_ => HNil)
        }
    }

  implicit def hconsDecodeJson[K <: Symbol, H, T <: HList]
   (implicit
     key: Witness.Aux[K],
     headDecode: Lazy[Decoder[H]],
     tailDecode: HListProductDecodeJson[T]
   ): HListProductDecodeJson[FieldType[K, H] :: T] =
    new HListProductDecodeJson[FieldType[K, H] :: T] {
      def apply(productCodec: JsonProductCodec) =
        new ADecoder[FieldType[K, H] :: T] {
          lazy val tailDecode0 = tailDecode(productCodec)

          def apply(a: ACursor) =
            for {
              x <- productCodec.decodeField(key.value.name, a, headDecode.value)
              (h, remaining) = x
              t <- tailDecode0(remaining)
            } yield field[K](h) :: t
        }
    }
}

object ProductDecodeJson {
  def apply[P](implicit decodeJson: ProductDecodeJson[P]): ProductDecodeJson[P] = decodeJson

  implicit def recordDecodeJson[R <: HList]
   (implicit
     underlying: HListProductDecodeJson[R]
   ): ProductDecodeJson[R] =
    new ProductDecodeJson[R] {
      def apply(productCodec: JsonProductCodec) =
        Decoder.instance { c =>
          productCodec.decodeStart(c).flatMap(_.as(underlying.decoder(productCodec)))
        }
    }

  implicit def genericDecodeJson[P, L <: HList]
   (implicit
     gen: LabelledGeneric.Aux[P, L],
     underlying: Lazy[HListProductDecodeJson[L]]
   ): ProductDecodeJson[P] =
    new ProductDecodeJson[P] {
      def apply(productCodec: JsonProductCodec) =
        Decoder.instance { c =>
          productCodec.decodeStart(c).flatMap(_.as(underlying.value.decoder(productCodec).map(gen.from)))
        }
    }
}

trait SumDecodeJson[S] {
  def apply(sumCodec: JsonSumCodec): Decoder[S]
}

trait CoproductSumDecodeJson[C <: Coproduct] {
  def apply(sumCodec: JsonSumCodec): Decoder[C]
}

object CoproductSumDecodeJson {
  def apply[C <: Coproduct](implicit decodeJson: CoproductSumDecodeJson[C]): CoproductSumDecodeJson[C] =
    decodeJson

  implicit def cnilDecodeJson: CoproductSumDecodeJson[CNil] =
    new CoproductSumDecodeJson[CNil] {
      def apply(sumCodec: JsonSumCodec) =
        new Decoder[CNil] {
          def apply(c: HCursor) =
            sumCodec.decodeEmpty(c)
        }
    }

  implicit def cconsDecodeJson[K <: Symbol, H, T <: Coproduct]
   (implicit
     key: Witness.Aux[K],
     headDecode: Lazy[MkDecodeJson[H]],
     tailDecode: CoproductSumDecodeJson[T]
   ): CoproductSumDecodeJson[FieldType[K, H] :+: T] =
    new CoproductSumDecodeJson[FieldType[K, H] :+: T] {
      def apply(sumCodec: JsonSumCodec) =
        new Decoder[FieldType[K, H] :+: T] {
          lazy val tailDecode0 = tailDecode(sumCodec)

          def apply(c: HCursor) =
            sumCodec.decodeField(key.value.name, c, headDecode.value.decodeJson).flatMap {
              case Left(tailCursor) => tailCursor.as(tailDecode0).map(Inr(_))
              case Right(h) => Xor.right(Inl(field[K](h)))
            }
        }
    }
}

object SumDecodeJson {
  def apply[S](implicit decodeJson: SumDecodeJson[S]): SumDecodeJson[S] = decodeJson

  implicit def unionDecodeJson[U <: Coproduct]
   (implicit
     underlying: CoproductSumDecodeJson[U]
   ): SumDecodeJson[U] =
    new SumDecodeJson[U] {
      def apply(sumCodec: JsonSumCodec) =
        underlying(sumCodec)
    }

  implicit def genericDecodeJson[S, C <: Coproduct]
   (implicit
     gen: LabelledGeneric.Aux[S, C],
     underlying: Lazy[CoproductSumDecodeJson[C]]
   ): SumDecodeJson[S] =
    new SumDecodeJson[S] {
      def apply(sumCodec: JsonSumCodec) =
        underlying.value(sumCodec)
          .map(gen.from)
    }
}


object MkDecodeJson {
  def apply[T](implicit decodeJson: MkDecodeJson[T]): MkDecodeJson[T] = decodeJson

  implicit def productDecodeJson[P]
   (implicit
     underlying: ProductDecodeJson[P],
     codecFor: JsonProductCodecFor[P]
   ): MkDecodeJson[P] =
    new MkDecodeJson[P] {
      def decodeJson = underlying(codecFor.codec)
    }

  implicit def sumDecodeJson[S]
   (implicit
     underlying: SumDecodeJson[S],
     codecFor: JsonSumCodecFor[S]
   ): MkDecodeJson[S] =
    new MkDecodeJson[S] {
      def decodeJson = underlying(codecFor.codec)
    }
}
