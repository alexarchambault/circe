package io.circe

import cats.data.Xor
import shapeless.{ Witness, Widen }

/**
 * Provides codecs for singletons types, corresponding to constant values.
 *
 * These are encoded as the corresponding parent type. Decoding ensures the decoded value
 * matches the singleton type one.
 *
 * E.g. with generic auto codecs
 * {{{
 *   import shapeless.Witness
 *   import shapeless.syntax.singleton._ // for narrow below
 *
 *   case class Obj(
 *     message: Witness.`"continue"`.T = "continue".narrow
 *   )
 *
 *   Obj().asJson.nospaces == """{"message":"continue"}"""
 * }}}
 *
 * @author Alexandre Archambault
 */
object singletons {

  implicit def singletonTypeEncoder[S, W >: S]
   (implicit
     w: Witness.Aux[S],
     widen: Widen.Aux[S, W],
     underlying: Encoder[W]
   ): Encoder[S] =
    underlying.contramap[S](widen.apply)

  implicit def singletonTypeDecoder[S, W >: S]
   (implicit
     w: Witness.Aux[S],
     widen: Widen.Aux[S, W],
     underlying: Decoder[W]
   ): Decoder[S] =
    Decoder.instance { c =>
      underlying(c).flatMap { w0 =>
        if (w0 == w.value)
          Xor.right(w.value)
        else
          Xor.left(DecodingFailure(s"Expected ${w.value}, got $w0", c.history))
      }
    }

}
