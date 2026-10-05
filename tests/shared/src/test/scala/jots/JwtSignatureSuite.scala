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
import jots.testing.*
import scodec.bits.Bases.Alphabets.Base64UrlNoPad
import weaver.SimpleIOSuite
import weaver.discipline.Discipline
import weaver.scalacheck.Checkers

object JwtSignatureSuite extends SimpleIOSuite with Checkers with Discipline {
  checkAll("JwtSignature.hash", HashTests[JwtSignature].hash)

  test("JwtSignature.show") {
    forall { (signature: JwtSignature) =>
      expect.eql(Show[JwtSignature].show(signature), signature.show) &&
      expect.eql(signature.show, signature.toBase64UrlNoPad)
    }
  }

  test("JwtSignature.toString") {
    forall { (signature: JwtSignature) =>
      expect(signature.toString.contains(signature.toBase64UrlNoPad))
    }
  }

  test("JwtSignature.fromBase64UrlNoPad") {
    forall { (signature: JwtSignature) =>
      expect.eql(Some(signature), JwtSignature.fromBase64UrlNoPad(signature.toBase64UrlNoPad).toOption)
    }
  }

  test("JwtSignature.fromBase64UrlNoPad.rejectNonCanonical") {
    forall { (signature: JwtSignature) =>
      val encoded = signature.toBase64UrlNoPad
      if (encoded.length % 4 == 0) success
      else {
        val nonCanonical = encoded.init :+ Base64UrlNoPad.toChar(Base64UrlNoPad.toIndex(encoded.last) | 1)
        expect(JwtSignature.fromBase64UrlNoPad(nonCanonical).isLeft)
      }
    }
  }
}
