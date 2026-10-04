/*
 * Copyright 2026 Viktor Rudebeck
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package jots.internal

import io.circe.CursorOp
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Error
import io.circe.Json
import io.circe.jawn.JawnParser
import scala.annotation.tailrec

/**
  * Used to limit the nesting depth of arrays and objects in `Json`.
  *
  * Deeply nested `Json` can overflow the stack when it is printed,
  * hashed or compared, since those operations are recursive.
  */
private[jots] object JsonDepth {

  /**
    * The maximum nesting depth of arrays and objects.
    */
  val Max: Int = 32

  /**
    * Returns `true` if the nesting depth of arrays and objects
    * in the specified `Json` exceeds [[Max]]; `false` otherwise.
    */
  def exceedsMax(json: Json): Boolean = {
    @tailrec
    def loop(stack: List[(Json, Int)]): Boolean =
      stack match {
        case (value, depth) :: rest =>
          value.asArray.map(_.toList).orElse(value.asObject.map(_.values.toList)) match {
            case Some(_) if depth > Max => true
            case Some(values) => loop(values.map((_, depth + 1)) ::: rest)
            case None => loop(rest)
          }
        case Nil =>
          false
      }

    loop(List((json, 1)))
  }

  /**
    * Parses the input using the parser and decodes the result, failing
    * if the nesting depth exceeds [[Max]] before attempting decoding.
    */
  def decode[A: Decoder](parser: JawnParser, input: CharSequence): Either[Error, A] =
    parser.parseCharSequence(input).flatMap { json =>
      if (exceedsMax(json)) Left(failure(Nil)) else json.as[A]
    }

  /**
    * Returns a `Decoder` which fails if the nesting depth exceeds
    * [[Max]] before attempting to decode using the decoder.
    */
  def decoder[A](decoder: Decoder[A]): Decoder[A] =
    Decoder.instance { cursor =>
      if (exceedsMax(cursor.value)) Left(failure(cursor.history))
      else decoder(cursor)
    }

  private def failure(history: List[CursorOp]): DecodingFailure =
    DecodingFailure(s"the nesting depth exceeds the maximum of $Max", history)
}
