package io.circe.generic

import io.circe.Decoder
import io.circe.generic.derive.MkDecodeJson
import shapeless.{ HList, LabelledGeneric }, shapeless.ops.function.FnFromProduct

trait IncompleteInstances {
  implicit def decodeIncompleteCaseClass[F, P <: HList, A, T <: HList, R <: HList](implicit
    ffp: FnFromProduct.Aux[P => A, F],
    gen: LabelledGeneric.Aux[A, T],
    complement: Complement.Aux[T, P, R],
    d: MkDecodeJson[R]
  ): Decoder[F] = d.decodeJson.map(r => ffp(p => gen.from(complement.insert(p, r))))

  implicit def decodeCaseClassPatch[A, R <: HList, O <: HList](implicit
    gen: LabelledGeneric.Aux[A, R],
    patch: PatchWithOptions.Aux[R, O],
    d: MkDecodeJson[O]
  ): Decoder[A => A] = d.decodeJson.map(o => a => gen.from(patch(gen.to(a), o)))
}
