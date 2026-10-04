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
  * The subjects (sub) accepted when verifying tokens.
  *
  * - [[AcceptedSubjects.any]] accepts tokens with any subject, and tokens
  *   without a subject. This is the default.
  * - `AcceptedSubjects(subject, subjects*)` accepts tokens with one of the
  *   subjects, while other tokens are rejected.
  */
sealed abstract class AcceptedSubjects {

  /**
    * Returns a `String` representation of the accepted subjects.
    */
  def show: String
}

object AcceptedSubjects {
  private[jots] case object AnySubject extends AcceptedSubjects {
    override val show: String = "AcceptedSubjects.any"
    override val toString: String = show
  }

  private[jots] final case class OneOf(subjects: NonEmptyList[String]) extends AcceptedSubjects {
    override def show: String = subjects.toList.mkString("AcceptedSubjects(", ", ", ")")
    override def toString: String = show
  }

  /**
    * Accepts tokens with any subject (sub), and tokens without
    * a subject. This is the default accepted subjects.
    */
  val any: AcceptedSubjects = AnySubject

  /**
    * Accepts tokens with one of the specified subjects (sub), while
    * tokens with other subjects or no subject are rejected.
    */
  def apply(subject: String, subjects: String*): AcceptedSubjects =
    fromList(NonEmptyList.of(subject, subjects: _*))

  /**
    * Accepts tokens with one of the specified subjects (sub), while
    * tokens with other subjects or no subject are rejected.
    */
  def fromList(subjects: NonEmptyList[String]): AcceptedSubjects =
    OneOf(subjects)

  implicit val acceptedSubjectsHash: Hash[AcceptedSubjects] =
    Hash.fromUniversalHashCode

  implicit val acceptedSubjectsShow: Show[AcceptedSubjects] =
    Show.show(_.show)
}
