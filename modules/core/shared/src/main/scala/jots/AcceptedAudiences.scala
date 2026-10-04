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
  * The audiences (aud) accepted when verifying tokens.
  *
  * - [[AcceptedAudiences.none]] rejects tokens with an audience, while
  *   tokens without an audience are accepted. This is the default.
  * - [[AcceptedAudiences.any]] accepts tokens with any audience, and
  *   tokens without an audience.
  * - `AcceptedAudiences(audience, audiences*)` accepts tokens with at
  *   least one of the audiences, while other tokens are rejected.
  *
  * Rejecting tokens with an audience by default follows the JWT
  * specification, which requires tokens to be rejected when the
  * audience is not recognized (RFC 7519 section 4.1.3).
  */
sealed abstract class AcceptedAudiences {

  /**
    * Returns a `String` representation of the accepted audiences.
    */
  def show: String
}

object AcceptedAudiences {
  private[jots] case object NoAudience extends AcceptedAudiences {
    override val show: String = "AcceptedAudiences.none"
    override val toString: String = show
  }

  private[jots] case object AnyAudience extends AcceptedAudiences {
    override val show: String = "AcceptedAudiences.any"
    override val toString: String = show
  }

  private[jots] final case class OneOf(audiences: NonEmptyList[String]) extends AcceptedAudiences {
    override def show: String = audiences.toList.mkString("AcceptedAudiences(", ", ", ")")
    override def toString: String = show
  }

  /**
    * Rejects tokens with an audience (aud), while tokens without an
    * audience are accepted. This is the default accepted audiences.
    */
  val none: AcceptedAudiences = NoAudience

  /**
    * Accepts tokens with any audience (aud), and tokens without an
    * audience. Only use this if tokens are not issued for multiple
    * recipients, since tokens for other recipients are accepted.
    */
  val any: AcceptedAudiences = AnyAudience

  /**
    * Accepts tokens with at least one of the specified audiences (aud),
    * while tokens with other audiences or no audience are rejected.
    */
  def apply(audience: String, audiences: String*): AcceptedAudiences =
    fromList(NonEmptyList.of(audience, audiences: _*))

  /**
    * Accepts tokens with at least one of the specified audiences (aud),
    * while tokens with other audiences or no audience are rejected.
    */
  def fromList(audiences: NonEmptyList[String]): AcceptedAudiences =
    OneOf(audiences)

  implicit val acceptedAudiencesHash: Hash[AcceptedAudiences] =
    Hash.fromUniversalHashCode

  implicit val acceptedAudiencesShow: Show[AcceptedAudiences] =
    Show.show(_.show)
}
