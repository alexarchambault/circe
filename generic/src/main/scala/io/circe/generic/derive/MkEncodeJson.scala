package io.circe
package generic
package derive

import shapeless._
import shapeless.labelled.FieldType

trait MkEncodeJson[T] {
  def encodeJson: Encoder[T]
}

trait ProductEncodeJson[T] {
  def apply(productCodec: JsonProductCodec): Encoder[T]
}

trait HListProductEncodeJson[L <: HList] {
  def apply(productCodec: JsonProductCodec): Encoder[L]
}

object HListProductEncodeJson {
  def apply[L <: HList](implicit encodeJson: HListProductEncodeJson[L]): HListProductEncodeJson[L] =
    encodeJson

  implicit def hnilEncodeJson: HListProductEncodeJson[HNil] =
    new HListProductEncodeJson[HNil] {
      def apply(productCodec: JsonProductCodec) =
        new Encoder[HNil] {
          def apply(l: HNil) = productCodec.encodeEmpty
        }
    }

  implicit def hconsEncodeJson[K <: Symbol, H, T <: HList]
   (implicit
     key: Witness.Aux[K],
     headEncode: Lazy[Encoder[H]],
     tailEncode: HListProductEncodeJson[T]
   ): HListProductEncodeJson[FieldType[K, H] :: T] =
    new HListProductEncodeJson[FieldType[K, H] :: T] {
      def apply(productCodec: JsonProductCodec) =
        new Encoder[FieldType[K, H] :: T] {
          lazy val tailEncode0 = tailEncode(productCodec)

          def apply(l: FieldType[K, H] :: T) =
            productCodec.encodeField(
              key.value.name -> headEncode.value(l.head),
              tailEncode0(l.tail)
            )
        }
    }
}

object ProductEncodeJson {
  def apply[P](implicit encodeJson: ProductEncodeJson[P]): ProductEncodeJson[P] = encodeJson

  implicit def recordEncodeJson[R <: HList]
   (implicit
     underlying: HListProductEncodeJson[R]
   ): ProductEncodeJson[R] =
    new ProductEncodeJson[R] {
      def apply(productCodec: JsonProductCodec) =
        Encoder.instance { r =>
          productCodec.encodeEnd(underlying(productCodec).apply(r))
        }
    }

  implicit def genericEncodeJson[P, L <: HList]
   (implicit
     gen: LabelledGeneric.Aux[P, L],
     underlying: Lazy[HListProductEncodeJson[L]]
   ): ProductEncodeJson[P] =
    new ProductEncodeJson[P] {
      def apply(productCodec: JsonProductCodec) =
        Encoder.instance { r =>
          productCodec.encodeEnd(underlying.value(productCodec).contramap(gen.to).apply(r))
        }
    }
}


trait SumEncodeJson[S] {
  def apply(sumCodec: JsonSumCodec): Encoder[S]
}

trait CoproductSumEncodeJson[C <: Coproduct] {
  def apply(sumCodec: JsonSumCodec): Encoder[C]
}

object CoproductSumEncodeJson {
  def apply[C <: Coproduct](implicit encodeJson: CoproductSumEncodeJson[C]): CoproductSumEncodeJson[C] =
    encodeJson

  implicit def cnilEncodeJson: CoproductSumEncodeJson[CNil] =
    new CoproductSumEncodeJson[CNil] {
      def apply(sumCodec: JsonSumCodec) =
        new Encoder[CNil] {
          def apply(c: CNil) =
            sumCodec.encodeEmpty
        }
    }

  implicit def cconsEncodeJson[K <: Symbol, H, T <: Coproduct]
   (implicit
     key: Witness.Aux[K],
     headEncode: Lazy[MkEncodeJson[H]],
     tailEncode: CoproductSumEncodeJson[T]
   ): CoproductSumEncodeJson[FieldType[K, H] :+: T] =
    new CoproductSumEncodeJson[FieldType[K, H] :+: T] {
      def apply(sumCodec: JsonSumCodec) =
        new Encoder[FieldType[K, H] :+: T] {
          lazy val tailEncode0 = tailEncode(sumCodec)

          def apply(c: FieldType[K, H] :+: T) =
            c match {
              case Inl(h) =>
                sumCodec.encodeField(
                  Right(key.value.name -> headEncode.value.encodeJson(h))
                )
              case Inr(r) =>
                tailEncode0(r)
            }
        }
    }
}

object SumEncodeJson {
  def apply[S](implicit encodeJson: SumEncodeJson[S]): SumEncodeJson[S] = encodeJson

  implicit def unionEncodeJson[U <: Coproduct]
   (implicit
     underlying: CoproductSumEncodeJson[U]
   ): SumEncodeJson[U] =
    new SumEncodeJson[U] {
      def apply(sumCodec: JsonSumCodec) =
        underlying(sumCodec)
    }

  implicit def genericEncodeJson[S, C <: Coproduct]
   (implicit
     gen: LabelledGeneric.Aux[S, C],
     underlying: Lazy[CoproductSumEncodeJson[C]]
   ): SumEncodeJson[S] =
    new SumEncodeJson[S] {
      def apply(sumCodec: JsonSumCodec) =
        underlying.value(sumCodec)
          .contramap(gen.to)
    }
}


object MkEncodeJson {
  def apply[T](implicit encodeJson: MkEncodeJson[T]): MkEncodeJson[T] = encodeJson

  implicit def productEncodeJson[P]
   (implicit
     underlying: ProductEncodeJson[P],
     codecFor: JsonProductCodecFor[P]
   ): MkEncodeJson[P] =
    new MkEncodeJson[P] {
      def encodeJson = underlying(codecFor.codec)
    }

  implicit def sumEncodeJson[S]
   (implicit
     underlying: SumEncodeJson[S],
     codecFor: JsonSumCodecFor[S]
   ): MkEncodeJson[S] =
    new MkEncodeJson[S] {
      def encodeJson = underlying(codecFor.codec)
    }
}
