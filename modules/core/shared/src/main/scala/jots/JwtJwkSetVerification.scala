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

import cats.ApplicativeThrow
import jots.JwtException.InvalidKeyId
import jots.JwtException.MissingKey
import jots.JwtException.MissingKeyId

/**
  * [[JwtVerification]] using the keys in a [[JwkSet]], created
  * using the `jwkSet` functions on [[JwtVerificationBuilder]].
  */
sealed trait JwtJwkSetVerification[F[_]] extends JwtVerification[F] {

  /**
    * Returns the keys in the [[JwkSet]] that cannot be
    * used for verification, together with the reason.
    *
    * Note keys without a key id (kid), or not meant
    * for signature verification, are excluded here.
    */
  def unusableKeys: List[(Jwk, JwtException)]
}

object JwtJwkSetVerification {

  /**
    * Returns a new [[JwtJwkSetVerification]] that verifies tokens
    * using the verification of the key matching the token key id.
    */
  private[jots] def fromKeyVerifications[F[_]](
    verifications: Map[JwkKeyId, JwtVerification[F]],
    unusable: List[(Jwk, JwtException)]
  )(implicit F: ApplicativeThrow[F]): JwtJwkSetVerification[F] =
    new JwtJwkSetVerification[F] {
      override val unusableKeys: List[(Jwk, JwtException)] =
        unusable

      override def verify(jwt: SignedJwt): F[VerifiedJwt] =
        jwt.header.toJsonObject("kid") match {
          case Some(keyId) =>
            keyId.asString.map(JwkKeyId.fromString) match {
              case Some(keyId) =>
                verifications.get(keyId) match {
                  case Some(verification) => verification.verify(jwt)
                  case None => F.raiseError(new MissingKey())
                }
              case None =>
                F.raiseError(new InvalidKeyId())
            }
          case None =>
            F.raiseError(new MissingKeyId())
        }
    }
}
