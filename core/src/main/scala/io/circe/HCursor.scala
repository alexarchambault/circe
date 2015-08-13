package io.circe

import cats.data.Xor
import io.circe.cursor.HCursorOperations

/**
 * A cursor that tracks the history of operations performed with it.
 *
 * @groupname Ungrouped HCursor fields and operations
 * @groupprio Ungrouped 1
 *
 * @see [[GenericCursor]]
 * @author Travis Brown
 */
case class HCursor(cursor: Cursor, history: List[CursorOp]) extends HCursorOperations {
  /**
   * Create an [[ACursor]] for this cursor.
   */
  def acursor: ACursor = ACursor.ok(this)

  /**
   * Create a failed [[ACursor]] for this cursor.
   */
  def failedACursor: ACursor = ACursor.fail(this)

  /**
   * Traverse taking `op` at each step, performing `f` on the current cursor and
   * accumulating `A`.
   *
   * This operation does not consume stack at each step, so is safe to work with
   * large structures (in contrast with recursively binding).
   */
  def traverseDecode[A](init: A)(
    op: HCursor => ACursor,
    f: (A, HCursor) => Xor[DecodingFailure, A]
  ): Xor[DecodingFailure, A] = loop[(HCursor, A), A](
    f(init, this).map(a => (this, a)),
    { case (c, acc) =>
        op(c).success.fold[Xor[Xor[DecodingFailure, A], Xor[DecodingFailure, (HCursor, A)]]](
          Xor.left(Xor.right[DecodingFailure, A](acc))
        )(hcursor =>
          Xor.right(f(acc, hcursor).map(b => (hcursor, b)))
        )
    }
  )

  private[this] final def loop[A, B](
    r1: Xor[DecodingFailure, A],
    f: A => Xor[Xor[DecodingFailure, B], Xor[DecodingFailure, A]]
  ): Xor[DecodingFailure, B] =
    r1.flatMap(a => f(a).swap.valueOr(r2 => loop[A, B](r2, f)))
}
