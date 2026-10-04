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

object AcceptedAudiencesSuite extends SimpleIOSuite with Checkers with Discipline {
  checkAll("AcceptedAudiences.hash", HashTests[AcceptedAudiences].hash)

  test("AcceptedAudiences.fromList") {
    forall { (audience: String, audiences: List[String]) =>
      expect.eql(
        AcceptedAudiences(audience, audiences: _*),
        AcceptedAudiences.fromList(NonEmptyList(audience, audiences))
      )
    }
  }

  test("AcceptedAudiences.show") {
    forall { (audiences: AcceptedAudiences) =>
      expect.eql(Show[AcceptedAudiences].show(audiences), audiences.show)
    }
  }

  pureTest("AcceptedAudiences.toString") {
    expect.eql("AcceptedAudiences.none", AcceptedAudiences.none.toString) &&
    expect.eql("AcceptedAudiences.any", AcceptedAudiences.any.toString) &&
    expect.eql("AcceptedAudiences(a, b)", AcceptedAudiences("a", "b").toString)
  }
}
