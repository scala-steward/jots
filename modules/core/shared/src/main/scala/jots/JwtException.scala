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

import cats.data.NonEmptyList
import cats.syntax.all.*
import io.circe.Json
import scala.concurrent.duration.FiniteDuration
import scala.util.control.NoStackTrace

/**
  * Exception raised when there is an issue relating to JWTs.
  */
abstract class JwtException(val message: String, val cause: Option[Throwable] = None)
  extends RuntimeException(message, cause.orNull)
  with NoStackTrace

object JwtException {

  /**
    * Exception raised when creating a [[JwtVerification]] with a
    * [[JwkSet]] that contains no keys for signature verification.
    */
  final class EmptyKeySet private[jots] (causes: List[JwtException] = Nil)
    extends JwtException(
      if (causes.isEmpty) "the key set has no keys for signature verification"
      else causes.map(_.message).mkString("the key set has no keys for signature verification: ", "; ", "")
    )

  /**
    * Exception raised by [[JwtVerification]] when a
    * token `alg` algorithm is not a `String`.
    */
  final class InvalidAlgorithm private[jots] () extends JwtException("the token algorithm (alg) is invalid")

  /**
    * Exception raised when the `aud` audience is not a `String` or array of `String`s.
    */
  final class InvalidAudience private[jots] (audience: Json)
    extends JwtException(s"the token audience (aud) [${audience.noSpaces}] is invalid")

  /**
    * Exception raised when the `crit` critical headers are not a non-empty array of `String`s.
    */
  final class InvalidCriticalHeaders private[jots] (criticalHeaders: Json)
    extends JwtException(s"the critical headers (crit) [${criticalHeaders.noSpaces}] are invalid")

  /**
    * Exception raised when an EC private key for [[JwtSigning]] or public
    * key for [[JwtVerification]] does not have the exact curve bit length
    * required by the accepted ECDSA algorithms.
    */
  final class InvalidEcKeyLength private[jots] (bitLength: Int)
    extends JwtException(
      s"the ECDSA key length was $bitLength bits, which did not match any algorithm"
    )

  /**
    * Exception raised when an EdDSA private key for [[JwtSigning]] or public
    * key for [[JwtVerification]] does not have the exact curve bit length
    * required by the accepted EdDSA algorithms.
    */
  final class InvalidEddsaKeyLength private[jots] (bitLength: Int)
    extends JwtException(
      s"the EdDSA key length was $bitLength bits, which did not match any algorithm"
    )

  /**
    * Exception raised when the `exp` expiration is not a valid expiration.
    */
  final class InvalidExpiration private[jots] (expiration: Json)
    extends JwtException(s"the token expiration (exp) [${expiration.noSpaces}] is invalid")

  /**
    * Exception raised by [[JwtSigning]] or [[JwtVerification]] when
    * the `alg` algorithm of a provided [[Jwk]] is not a `String`.
    */
  final class InvalidKeyAlgorithm private[jots] (keyId: Option[JwkKeyId], algorithm: Json)
    extends JwtException({
      val id = keyId.foldMap(keyId => s" with id [${keyId.value}]")
      s"the key$id has invalid algorithm (alg) [${algorithm.noSpaces}]"
    })

  /**
    * Exception raised when the `kid` key identifier is not a `String`.
    */
  final class InvalidKeyId private[jots] () extends JwtException("the key id (kid) is invalid")

  /**
    * Exception raised when the `kty` key type is not a `String`.
    */
  final class InvalidKeyType private[jots] (keyType: Json)
    extends JwtException(s"the key type (kty) [${keyType.noSpaces}] is invalid")

  /**
    * Exception raised when the `iat` issued-at is not a valid value.
    */
  final class InvalidIssuedAt private[jots] (issuedAt: Json)
    extends JwtException(s"the token issued at (iat) [${issuedAt.noSpaces}] is invalid")

  /**
    * Exception raised when the `iss` issuer is not a `String`.
    */
  final class InvalidIssuer private[jots] (issuer: Json)
    extends JwtException(s"the token issuer (iss) [${issuer.noSpaces}] is invalid")

  /**
    * Exception raised when parsing a JSON Web Key (JWK) fails.
    */
  final class InvalidJwk private[jots] (details: String, cause: Option[Throwable] = None)
    extends JwtException(s"the Json Web Key (JWK) is invalid: $details", cause)

  /**
    * Exception raised when parsing a JSON Web Key Set (JWK Set) fails.
    */
  final class InvalidJwkSet private[jots] (details: String, cause: Option[Throwable] = None)
    extends JwtException(s"the Json Web Key Set (JWK Set) is invalid: $details", cause)

  /**
    * Exception raised when parsing a JWT signature fails.
    */
  final class InvalidJwtSignature private[jots] (details: String)
    extends JwtException(s"the signature is invalid: $details")

  /**
    * Exception raised when the `nbf` not-before is not a valid value.
    */
  final class InvalidNotBefore private[jots] (notBefore: Json)
    extends JwtException(s"the token not-before (nbf) [${notBefore.noSpaces}] is invalid")

  /**
    * Exception raised when a private key is invalid, for example when
    * [[Jwk#toPrivateKey]] fails to generate a key, or when the length
    * of a private key for [[JwtSigning]] cannot be determined.
    */
  final class InvalidPrivateKey private[jots] (details: String)
    extends JwtException(s"invalid private key: $details")

  /**
    * Exception raised when a public key is invalid, for example when
    * [[Jwk#toPublicKey]] fails to generate a key, or when the length
    * of a public key for [[JwtVerification]] cannot be determined.
    */
  final class InvalidPublicKey private[jots] (details: String)
    extends JwtException(s"invalid public key: $details")

  /**
    * Exception raised when an RSA private key for [[JwtSigning]]
    * or public key for [[JwtVerification]] is less than 2048 bits.
    */
  final class InvalidRsaKeyLength private[jots] (bitLength: Int)
    extends JwtException(s"the RSA key length was $bitLength bits, less than the minimum 2048 bits")

  /**
    * Exception raised when [[Jwk#toSecretKey]] fails to generate a key.
    */
  final class InvalidSecretKey private[jots] (details: String)
    extends JwtException(s"invalid secret key: $details")

  /**
    * Exception raised when a secret key for [[JwtSigning]]
    * or [[JwtVerification]] is less than the minimum key
    * length set by [[JwtHmacAlgorithm#minKeyLength]].
    */
  final class InvalidSecretKeyLength private[jots] (algorithm: JwtHmacAlgorithm)
    extends JwtException(
      s"the secret key length was less than required for $algorithm (${algorithm.minKeyLength} bytes)"
    )

  /**
    * Exception raised when [[JwtVerification]] fails to verify the token signature.
    */
  final class InvalidSignature private[jots] ()
    extends JwtException("the token signature did not match the expected signature")

  /**
    * Exception raised when parsing a signed JWT fails.
    */
  final class InvalidSignedJwt private[jots] (details: Option[String] = None, cause: Option[Throwable] = None)
    extends JwtException(s"the token is invalid${details.foldMap(details => s": $details")}", cause)

  /**
    * Exception raised when parsing JWT claims fails.
    */
  final class InvalidSignedJwtClaims private[jots] (details: String, cause: Option[Throwable] = None)
    extends JwtException(s"the claims are invalid: $details", cause)

  /**
    * Exception raised when parsing a JWT header fails.
    */
  final class InvalidSignedJwtHeader private[jots] (details: String, cause: Option[Throwable] = None)
    extends JwtException(s"the header is invalid: $details", cause)

  /**
    * Exception raised when the `sub` subject is not a `String`.
    */
  final class InvalidSubject private[jots] (subject: Json)
    extends JwtException(s"the token subject (sub) [${subject.noSpaces}] is invalid")

  /**
    * Exception raised when the `typ` type is not a `String`.
    */
  final class InvalidType private[jots] (`type`: Json)
    extends JwtException(s"the token type (typ) [${`type`.noSpaces}] is invalid")

  /**
    * Exception raised by [[JwtVerification]] when a token is missing the `alg` algorithm.
    */
  final class MissingAlgorithm private[jots] ()
    extends JwtException("the token header is missing the algorithm (alg)")

  /**
    * Exception raised by [[JwtVerification]] when a token is missing the `aud` audience.
    */
  final class MissingAudience private[jots] ()
    extends JwtException("the token claims are missing the audience (aud)")

  /**
    * Exception raised by [[JwtVerification]] when a `crit` critical header
    * name is not also present as a header parameter in the token header.
    */
  final class MissingCriticalHeader private[jots] (criticalHeader: String)
    extends JwtException(s"the critical header [$criticalHeader] is missing from the token header")

  /**
    * Exception raised by [[JwtVerification]] when a required `exp` expiration is missing.
    */
  final class MissingExpiration private[jots] ()
    extends JwtException("the token claims are missing the expiration (exp)")

  /**
    * Exception raised by [[JwtVerification]] when a required `iat` issued-at is missing.
    */
  final class MissingIssuedAt private[jots] ()
    extends JwtException("the token claims are missing the issued at (iat)")

  /**
    * Exception raised by [[JwtVerification]] when a token is missing the `iss` issuer.
    */
  final class MissingIssuer private[jots] ()
    extends JwtException("the token claims are missing the issuer (iss)")

  /**
    * Exception raised by [[JwtVerification]] when no key in the key set matches the token key id (kid).
    */
  final class MissingKey private[jots] ()
    extends JwtException("the key set has no key matching the token key id (kid)")

  /**
    * Exception raised when a [[Jwk]] or token is missing the `kid` property.
    */
  final class MissingKeyId private[jots] ()
    extends JwtException("the key or token is missing the key id (kid)")

  /**
    * Exception raised when a [[Jwk]] is missing the `kty` key type.
    */
  final class MissingKeyType private[jots] () extends JwtException("the key is missing the key type (kty)")

  /**
    * Exception raised by [[JwtVerification]] when a required `nbf` not-before is missing.
    */
  final class MissingNotBefore private[jots] ()
    extends JwtException("the token claims are missing the not-before (nbf)")

  /**
    * Exception raised when a token is missing the `sub` subject.
    */
  final class MissingSubject private[jots] ()
    extends JwtException("the token claims are missing the subject (sub)")

  /**
    * Exception raised by [[JwtVerification]] when a token is missing the `typ` type.
    */
  final class MissingType private[jots] () extends JwtException("the token header is missing the type (typ)")

  /**
    * Exception raised by [[JwtVerification]] when there are no accepted
    * algorithms for a provided [[Jwk]] with the specified [[JwkKeyType]].
    */
  final class NoAcceptedAlgorithms private[jots] (keyId: JwkKeyId, keyType: JwkKeyType)
    extends JwtException(
      s"no accepted algorithms for key with id [${keyId.value}] and type [${keyType.name}]"
    )

  /**
    * Exception raised by [[JwtVerification]] when a token `alg` algorithm is
    * not one of the algorithms the verification has been configured to accept.
    */
  final class RejectedAlgorithm private[jots] ()
    extends JwtException("the token algorithm (alg) was rejected")

  /**
    * Exception raised by [[JwtVerification]] when a token `aud` audience is rejected.
    */
  final class RejectedAudience private[jots] (audience: List[String])
    extends JwtException(s"the token audience (aud) [${audience.mkString(",")}] was rejected")

  /**
    * Exception raised by [[JwtVerification]] when a token `iss` issuer is rejected.
    */
  final class RejectedIssuer private[jots] (issuer: String)
    extends JwtException(s"the token issuer (iss) [$issuer] was rejected")

  /**
    * Exception raised by [[JwtSigning]] or [[JwtVerification]] when the `alg`
    * algorithm of a provided [[Jwk]] is not one of the algorithms the signing
    * or verification has been configured to use.
    */
  final class RejectedKeyAlgorithm private[jots] (
    keyId: Option[JwkKeyId],
    keyAlgorithm: String,
    algorithms: NonEmptyList[JwtAlgorithm]
  ) extends JwtException({
      val id = keyId.foldMap(keyId => s" id [${keyId.value}] and")
      val expected = algorithms.toList.map(_.name).mkString(",")
      s"the key with$id algorithm (alg) [$keyAlgorithm] " +
        s"was rejected, expected [$expected]"
    })

  /**
    * Exception raised by [[JwtVerification]] when a token `sub` subject is rejected.
    */
  final class RejectedSubject private[jots] (subject: String)
    extends JwtException(s"the token subject (sub) [$subject] was rejected")

  /**
    * Exception raised by [[JwtVerification]] when a token `typ` type is rejected.
    */
  final class RejectedType private[jots] (`type`: String)
    extends JwtException(s"the token type (typ) [${`type`}] was rejected")

  /**
    * Exception raised by [[JwtVerification]] when the token signature could
    * not be verified, for example because the public key could not be used.
    */
  final class SignatureVerificationFailed private[jots] (cause: Throwable)
    extends JwtException("the token signature could not be verified", Some(cause))

  /**
    * Exception raised when the `exp` expiration time has been reached.
    */
  final class TokenExpired private[jots] (expiresAt: FiniteDuration)
    extends JwtException(s"the token expired at ${expiresAt.toSeconds} epoch seconds")

  /**
    * Exception raised when the `iat` issued-at time has not been reached.
    */
  final class TokenNotYetIssued private[jots] (issuedAt: FiniteDuration)
    extends JwtException(s"the token is not issued before ${issuedAt.toSeconds} epoch seconds")

  /**
    * Exception raised when the `nbf` not-before time has not been reached.
    */
  final class TokenNotYetValid private[jots] (notBefore: FiniteDuration)
    extends JwtException(s"the token is not valid before ${notBefore.toSeconds} epoch seconds")

  /**
    * Exception raised by [[JwtVerification]] when a token has an `aud` audience
    * but the accepted audiences are [[AcceptedAudiences.none]], which is the
    * default when no accepted audiences have been set.
    */
  final class UnexpectedAudience private[jots] (audience: Json)
    extends JwtException(
      s"the token audience (aud) [${audience.noSpaces}] was rejected since no audiences are accepted"
    )

  /**
    * Exception raised when creating a [[JwtSigning]] with a [[Jwk]] that is
    * not suitable for signing, as detailed by the specified `details`.
    */
  final class UnsuitableSigningKey private[jots] (keyId: Option[JwkKeyId], details: String)
    extends JwtException({
      val id = keyId.foldMap(keyId => s" with id [${keyId.value}]")
      s"the key${id} is not suitable for signing: $details"
    })

  /**
    * Exception raised by [[JwtVerification]] when a token `crit` header lists a
    * critical header that the verification has not been configured to accept.
    */
  final class UnsupportedCriticalHeader private[jots] (criticalHeader: String)
    extends JwtException(s"the critical header [$criticalHeader] is not supported")

  /**
    * Exception raised by [[JwtSigning]] or [[JwtVerification]]
    * when a provided [[Jwk]] is not supported by the library.
    */
  final class UnsupportedKey private[jots] (
    keyId: Option[JwkKeyId],
    keyType: JwkKeyType,
    algorithm: Option[JwtAlgorithm] = None
  ) extends JwtException({
      val andId = keyId.foldMap(keyId => s" and id [${keyId.value}]")
      val andAlgorithm = algorithm.foldMap(algorithm => s" and algorithm [${algorithm.name}]")
      s"the key with type [${keyType.name}]${andId}${andAlgorithm} is not supported"
    })

  /**
    * Exception raised when a [[JwkKeyType]] is not recognized.
    */
  final class UnsupportedKeyType private[jots] (keyType: String)
    extends JwtException(s"the key type (kty) [$keyType] is not supported")
}
