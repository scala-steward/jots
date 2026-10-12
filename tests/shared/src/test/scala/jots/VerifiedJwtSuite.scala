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

import cats.kernel.laws.discipline.HashTests
import jots.testing.*
import weaver.SimpleIOSuite
import weaver.discipline.Discipline
import weaver.scalacheck.Checkers

object VerifiedJwtSuite extends SimpleIOSuite with Checkers with Discipline {
  test("VerifiedJwt.fromVerified") {
    // The testing module calls this package-private function,
    // which MiMa does not check, so its signature must remain
    // binary compatible.
    val fromVerified: SignedJwt => VerifiedJwt =
      VerifiedJwt.fromVerified

    forall { (jwt: SignedJwt) =>
      expect.eql(jwt, fromVerified(jwt).toSigned)
    }
  }

  checkAll("VerifiedJwt.hash", HashTests[VerifiedJwt].hash)

  test("VerifiedJwt.show") {
    forall { (jwt: VerifiedJwt) =>
      expect.eql(jwt.show, jwt.toSigned.show)
    }
  }
}
