package io.circe
package generic

import cats.data.Xor
import io.circe.generic.derive.Widen
import shapeless.Witness

trait SingletonInstances {

  implicit def singletonTypeEncodeJson[S, W]
   (implicit
     w: Witness.Aux[S],
     widen: Widen.Aux[S, W],
     underlying: Encoder[W]
   ): Encoder[S] =
    underlying.contramap[S](widen.to)

  implicit def singletonTypeDecodeJson[S, W]
   (implicit
     w: Witness.Aux[S],
     widen: Widen.Aux[S, W],
     underlying: Decoder[W]
   ): Decoder[S] =
    Decoder.instance { c =>
      underlying(c).flatMap { w0 =>
        widen.from(w0) match {
          case Some(s) => Xor.right(s)
          case None => Xor.left(DecodingFailure(s"Expected ${w.value}, got $w0", c.history))
        }
      }
    }

}
