package io.circe
package generic
package derive

import cats.data.Xor

trait JsonSumCodec {
  def encodeEmpty: Nothing
  def encodeField(fieldOrObj: Either[Json, (String, Json)]): Json

  def decodeEmpty(cursor: HCursor): Xor[DecodingFailure, Nothing]
  def decodeField[A](name: String, cursor: HCursor, decode: Decoder[A]): Xor[DecodingFailure, Either[ACursor, A]]
}

trait JsonSumCodecFor[S] {
  def codec: JsonSumCodec
}

object JsonSumCodecFor {
  def apply[S](codec0: JsonSumCodec): JsonSumCodecFor[S] =
    new JsonSumCodecFor[S] {
      def codec = codec0
    }
}

object JsonSumCodec {
  val obj = JsonSumObjCodec()
  val typeField = JsonSumTypeFieldCodec()
}

case class JsonSumObjCodec(
  toJsonName: Option[String => String] = None
) extends JsonSumCodec {
  private def toJsonName0(name: String) =
    toJsonName.fold(name)(_(name))


  def encodeEmpty: Nothing =
    throw new IllegalArgumentException("empty")
  def encodeField(fieldOrObj: Either[Json, (String, Json)]): Json =
    fieldOrObj match {
      case Left(other) => other
      case Right((name, content)) =>
        Json.obj(toJsonName0(name) -> content)
    }

  def decodeEmpty(cursor: HCursor): Xor[DecodingFailure, Nothing] =
    Xor.Left(DecodingFailure(s"unrecognized type(s): ${cursor.fields.getOrElse(Nil).mkString(", ")}", cursor.history))
  def decodeField[A](name: String, cursor: HCursor, decode: Decoder[A]): Xor[DecodingFailure, Either[ACursor, A]] =
    cursor.downField(toJsonName0(name)).either match {
      case Xor.Left(_) =>
        Xor.right(Left(ACursor.ok(cursor)))
      case Xor.Right(content) =>
        decode(content).map(Right(_))
    }
}

case class JsonSumTypeFieldCodec(
  typeField: String = "$type",
  toTypeValue: Option[String => String] = None
) extends JsonSumCodec {
  private def toTypeValue0(name: String) =
    toTypeValue.fold(name)(_(name))

  def encodeEmpty: Nothing =
    throw new IllegalArgumentException("empty")
  def encodeField(fieldOrObj: Either[Json, (String, Json)]): Json =
    fieldOrObj match {
      case Left(other) => other
      case Right((name, content)) =>
        content.withObject(o => Json.fromJsonObject((typeField -> Json.string(toTypeValue0(name))) +: o))
    }

  def decodeEmpty(cursor: HCursor): Xor[DecodingFailure, Nothing] =
    Xor.Left(DecodingFailure(
      cursor.downField(typeField).focus match {
        case None => "no type found"
        case Some(type0) => s"unrecognized type: $type0"
      },
      cursor.history
    ))
  def decodeField[A](name: String, cursor: HCursor, decode: Decoder[A]): Xor[DecodingFailure, Either[ACursor, A]] =
    cursor.downField(typeField).as[String] match {
      case Xor.Right(name0) if toTypeValue0(name) == name0 =>
        decode(cursor).map(Right(_))
      case _ =>
        Xor.right(Left(ACursor.ok(cursor)))
    }
}
