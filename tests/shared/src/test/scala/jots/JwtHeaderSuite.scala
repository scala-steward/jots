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
import cats.syntax.all.*
import io.circe.JsonObject
import io.circe.syntax.*
import jots.testing.*
import weaver.SimpleIOSuite
import weaver.discipline.Discipline
import weaver.scalacheck.Checkers

object JwtHeaderSuite extends SimpleIOSuite with Checkers with Discipline {
  pureTest("JwtHeader.empty") {
    expect.eql(JsonObject.empty, JwtHeader.empty.toJsonObject)
  }

  pureTest("JwtHeader.default") {
    expect.eql(JsonObject("typ" -> "JWT".asJson), JwtHeader.default.toJsonObject)
  }

  checkAll("JwtHeader.hash", HashTests[JwtHeader].hash)

  test("JwtHeader.show") {
    forall { (claims: JwtHeader) =>
      expect.eql(Show[JwtHeader].show(claims), claims.show) &&
      expect.eql(claims.show, claims.toBase64UrlNoPad)
    }
  }

  test("JwtHeader.addOption") {
    forall { (header: JwtHeader, name: String, value: String) =>
      val added = header.add(name, value.asJson)
      expect.eql(added, header.addOption(name, Some(value.asJson))) &&
      expect.eql(added.remove(name), added.addOption(name, None))
    }
  }

  test("JwtHeader.withCriticalHeaders") {
    forall { (header: JwtHeader, criticalHeader: String, criticalHeaders: List[String]) =>
      val nonEmpty = NonEmptyList(criticalHeader, criticalHeaders)
      expect.eql(Some(nonEmpty.asJson), header.withCriticalHeaders(nonEmpty).toJsonObject("crit")) &&
      expect.eql(
        header.withCriticalHeaders(nonEmpty),
        header.withCriticalHeaders(criticalHeader, criticalHeaders: _*)
      )
    }
  }

  test("JwtHeader.toString") {
    forall { (claims: JwtHeader) =>
      claims.toJsonObject.toList.map { case (key, value) =>
        expect(claims.toString.contains(key)) &&
        expect(claims.toString.contains(value.show))
      }.combineAll
    }
  }
}
