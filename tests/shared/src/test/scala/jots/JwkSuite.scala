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
import cats.kernel.laws.discipline.HashTests
import io.circe.Json
import io.circe.syntax.*
import jots.testing.*
import org.scalacheck.Gen
import weaver.SimpleIOSuite
import weaver.discipline.Discipline
import weaver.scalacheck.Checkers

object JwkSuite extends SimpleIOSuite with Checkers with Discipline {
  checkAll("Jwk.hash", HashTests[Jwk].hash)

  test("Jwk.show") {
    forall { (jwk: Jwk) =>
      expect.eql(Show[Jwk].show(jwk), jwk.show) &&
      expect.eql(jwk.show, jwk.toString)
    }
  }

  test("Jwk.toJson") {
    forall { (jwk: Jwk) =>
      expect.eql(Right(jwk), jwk.toJson.as[Jwk])
    }
  }

  test("Jwk.toPublicJwk") {
    forall(Gen.oneOf(jwkEcdsaKeyPairGen, jwkEddsaKeyPairGen, jwkRsaKeyPairGen)) {
      case (privateKey, publicKey) =>
        expect.eql(Some(publicKey), privateKey.toPublicJwk.toOption) &&
        expect.eql(Some(publicKey), publicKey.toPublicJwk.toOption)
    }
  }

  test("Jwk.toPublicJwk.removesOtherPrimes") {
    forall(jwkRsaKeyPairGen) { case (privateKey, publicKey) =>
      val otherPrimes = Json.arr(Json.obj("r" -> "AQAB".asJson, "d" -> "AQAB".asJson, "t" -> "AQAB".asJson))
      val otherPrimesKey = Jwk.fromJsonObject(privateKey.toJsonObject.add("oth", otherPrimes))
      expect.eql(Some(publicKey), otherPrimesKey.flatMap(_.toPublicJwk).toOption)
    }
  }

  test("Jwk.toPublicJwk.rejectSecretKey") {
    forall(jwkOctGen) { jwk =>
      expect(jwk.toPublicJwk.isLeft)
    }
  }

  pureTest("Jwk.toPublicJwk.rejectUnknownKeyType") {
    val jwk = Jwk("kty" -> "AKP".asJson, "priv" -> "AQAB".asJson).fold(throw _, identity)
    expect(jwk.toPublicJwk.isLeft)
  }

  pureTest("Jwk.fromString.rejectNested") {
    expect(Jwk.fromString(s"""{"kty":"oct","nested":${"[" * 1000}${"]" * 1000}}""").isLeft)
  }

  pureTest("Jwk.decoder.rejectNested") {
    val nested = (1 to 1000).foldLeft(Json.arr())((json, _) => Json.arr(json))
    expect(Json.obj("kty" -> "oct".asJson, "nested" -> nested).as[Jwk].isLeft)
  }
}
