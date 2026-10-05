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

package jots

import cats.Hash
import cats.Show
import cats.data.NonEmptyList

/**
  * The types (typ) accepted when verifying tokens.
  *
  * - [[AcceptedTypes.any]] accepts tokens with any type, and tokens
  *   without a type. This is the default.
  * - `AcceptedTypes(type, types*)` accepts tokens with one of the
  *   types, while other tokens are rejected.
  *
  * Types are media types compared case-insensitively, where types
  * without a `/` have an implied `application/` prefix. This means
  * `JWT` accepts the types `JWT`, `jwt`, and `application/jwt`.
  */
sealed abstract class AcceptedTypes {

  /**
    * Returns a `String` representation of the accepted types.
    */
  def show: String
}

object AcceptedTypes {
  private[jots] case object AnyType extends AcceptedTypes {
    override val show: String = "AcceptedTypes.any"
    override val toString: String = show
  }

  private[jots] final case class OneOf(types: NonEmptyList[String]) extends AcceptedTypes {
    override def show: String = types.toList.mkString("AcceptedTypes(", ", ", ")")
    override def toString: String = show
  }

  /**
    * Accepts tokens with any type (typ), and tokens without
    * a type. This is the default accepted types.
    */
  val any: AcceptedTypes = AnyType

  /**
    * Accepts tokens with one of the specified types (typ), while
    * tokens with other types or no type are rejected.
    */
  def apply(`type`: String, types: String*): AcceptedTypes =
    fromList(NonEmptyList.of(`type`, types: _*))

  /**
    * Accepts tokens with one of the specified types (typ), while
    * tokens with other types or no type are rejected.
    */
  def fromList(types: NonEmptyList[String]): AcceptedTypes =
    OneOf(types)

  implicit val acceptedTypesHash: Hash[AcceptedTypes] =
    Hash.fromUniversalHashCode

  implicit val acceptedTypesShow: Show[AcceptedTypes] =
    Show.show(_.show)
}
