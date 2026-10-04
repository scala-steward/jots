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
  * The issuers (iss) accepted when verifying tokens.
  *
  * - [[AcceptedIssuers.any]] accepts tokens with any issuer, and tokens
  *   without an issuer. This is the default.
  * - `AcceptedIssuers(issuer, issuers*)` accepts tokens with one of the
  *   issuers, while other tokens are rejected.
  */
sealed abstract class AcceptedIssuers {

  /**
    * Returns a `String` representation of the accepted issuers.
    */
  def show: String
}

object AcceptedIssuers {
  private[jots] case object AnyIssuer extends AcceptedIssuers {
    override val show: String = "AcceptedIssuers.any"
    override val toString: String = show
  }

  private[jots] final case class OneOf(issuers: NonEmptyList[String]) extends AcceptedIssuers {
    override def show: String = issuers.toList.mkString("AcceptedIssuers(", ", ", ")")
    override def toString: String = show
  }

  /**
    * Accepts tokens with any issuer (iss), and tokens without
    * an issuer. This is the default accepted issuers.
    */
  val any: AcceptedIssuers = AnyIssuer

  /**
    * Accepts tokens with one of the specified issuers (iss), while
    * tokens with other issuers or no issuer are rejected.
    */
  def apply(issuer: String, issuers: String*): AcceptedIssuers =
    fromList(NonEmptyList.of(issuer, issuers: _*))

  /**
    * Accepts tokens with one of the specified issuers (iss), while
    * tokens with other issuers or no issuer are rejected.
    */
  def fromList(issuers: NonEmptyList[String]): AcceptedIssuers =
    OneOf(issuers)

  implicit val acceptedIssuersHash: Hash[AcceptedIssuers] =
    Hash.fromUniversalHashCode

  implicit val acceptedIssuersShow: Show[AcceptedIssuers] =
    Show.show(_.show)
}
