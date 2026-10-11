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

/**
  * [[JwtVerification]] using the keys in a [[JwkSet]], created
  * using the `jwkSet` functions on [[JwtVerificationBuilder]].
  */
trait JwtJwkSetVerification[F[_]] extends JwtVerification[F] {

  /**
    * Returns the keys in the [[JwkSet]] that cannot be
    * used for verification, together with the reason.
    *
    * Note keys without a key id (kid), or not meant
    * for signature verification, are excluded here.
    */
  def unusableKeys: List[(Jwk, JwtException)]
}
