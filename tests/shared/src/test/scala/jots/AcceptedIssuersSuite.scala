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

import cats.Show
import cats.data.NonEmptyList
import cats.kernel.laws.discipline.HashTests
import jots.testing.*
import weaver.SimpleIOSuite
import weaver.discipline.Discipline
import weaver.scalacheck.Checkers

object AcceptedIssuersSuite extends SimpleIOSuite with Checkers with Discipline {
  checkAll("AcceptedIssuers.hash", HashTests[AcceptedIssuers].hash)

  test("AcceptedIssuers.fromList") {
    forall { (issuer: String, issuers: List[String]) =>
      expect.eql(
        AcceptedIssuers(issuer, issuers: _*),
        AcceptedIssuers.fromList(NonEmptyList(issuer, issuers))
      )
    }
  }

  test("AcceptedIssuers.show") {
    forall { (issuers: AcceptedIssuers) =>
      expect.eql(Show[AcceptedIssuers].show(issuers), issuers.show)
    }
  }

  pureTest("AcceptedIssuers.toString") {
    expect.eql("AcceptedIssuers.any", AcceptedIssuers.any.toString) &&
    expect.eql("AcceptedIssuers(a, b)", AcceptedIssuers("a", "b").toString)
  }
}
