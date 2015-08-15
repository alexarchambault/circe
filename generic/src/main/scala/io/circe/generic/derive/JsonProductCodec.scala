package io.circe
package generic
package derive

import cats.data.Xor

trait JsonProductCodec {
  def encodeEmpty: Json
  def encodeField(field: (String, Json), obj: Json): Json
  def encodeEnd(obj: Json): Json

  def decodeStart(cursor: HCursor): Xor[DecodingFailure, ACursor]
  def decodeField[A](name: String, cursor: ACursor, decode: Decoder[A]): Xor[DecodingFailure, (A, ACursor)]
  def decodeEmpty(cursor: ACursor): Xor[DecodingFailure, Unit]
}

trait JsonProductCodecFor[P] {
  def codec: JsonProductCodec
}

object JsonProductCodecFor {
  def apply[S](codec0: JsonProductCodec): JsonProductCodecFor[S] =
    new JsonProductCodecFor[S] {
      def codec = codec0
    }
}

object JsonProductCodec {
  val obj = JsonProductObjCodec()
  val array = JsonProductArrayCodec
}

case class JsonProductObjCodec(
  toJsonName: Option[String => String] = None
) extends JsonProductCodec {
  def toJsonName0(name: String) = toJsonName.fold(name)(_(name))

  val encodeEmpty: Json = Json.obj()
  def encodeField(field: (String, Json), obj: Json): Json = {
    val (name, content) = field
    obj.withObject(o => Json.fromJsonObject((toJsonName0(name) -> content) +: o))
  }
  def encodeEnd(obj: Json): Json = obj

  def decodeStart(cursor: HCursor): Xor[DecodingFailure, ACursor] = Xor.right(cursor.acursor)
  def decodeField[A](name: String, cursor: ACursor, decode: Decoder[A]): Xor[DecodingFailure, (A, ACursor)] =
    cursor
      .downField(toJsonName0(name))
      .as(decode)
      .map((_, cursor))
  def decodeEmpty(cursor: ACursor): Xor[DecodingFailure, Unit] = Xor.right(())
}

case object JsonProductArrayCodec extends JsonProductCodec {
  val encodeEmpty: Json = Json.array()
  def encodeField(field: (String, Json), obj: Json): Json = {
    val (_, content) = field
    obj.withArray(a => Json.fromValues(content +: a))
  }
  def encodeEnd(a: Json): Json = a

  def decodeStart(cursor: HCursor): Xor[DecodingFailure, ACursor] = Xor.right(cursor.downArray)
  def decodeField[A](name: String, cursor: ACursor, decode: Decoder[A]): Xor[DecodingFailure, (A, ACursor)] =
    cursor
      .as(decode)
      .map((_, cursor.right))
  def decodeEmpty(cursor: ACursor): Xor[DecodingFailure, Unit] =
    if (cursor.succeeded)
      Xor.left(DecodingFailure("Too long array", cursor.history))
    else
      Xor.right(())
}
