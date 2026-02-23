package ua.gradsoft.epp

import java.io.ByteArrayInputStream
import java.nio.file.{Files, Path}
import java.security.{KeyFactory, KeyStore}
import java.security.cert.{CertificateFactory, X509Certificate}
import java.security.spec.PKCS8EncodedKeySpec
import java.util.Base64
import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory}

object EppSslContext {

  def fromPemFile(pemPath: String, serverCaPemPath: Option[String] = None): SSLContext = {
    val pemContent = Files.readString(Path.of(pemPath))
    val serverCaPemContent = serverCaPemPath.map(p => Files.readString(Path.of(p)))
    fromPemString(pemContent, serverCaPemContent)
  }

  def fromPemString(pemContent: String, serverCaPemContent: Option[String] = None): SSLContext = {
    val privateKey = extractPrivateKey(pemContent)
    val certificate = extractCertificate(pemContent)

    val keyStore = KeyStore.getInstance("PKCS12")
    keyStore.load(null, null)
    keyStore.setKeyEntry("client", privateKey, Array.emptyCharArray, Array(certificate))

    val kmf = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
    kmf.init(keyStore, Array.emptyCharArray)

    val trustManagers = serverCaPemContent.map { caPem =>
      val caCerts = extractAllCertificates(caPem)
      val trustStore = KeyStore.getInstance("PKCS12")
      trustStore.load(null, null)
      caCerts.zipWithIndex.foreach { (cert, idx) =>
        trustStore.setCertificateEntry(s"ca-$idx", cert)
      }
      val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
      tmf.init(trustStore)
      tmf.getTrustManagers
    }.orNull

    val sslContext = SSLContext.getInstance("TLS")
    sslContext.init(kmf.getKeyManagers, trustManagers, null)
    sslContext
  }

  private def extractPrivateKey(pem: String): java.security.PrivateKey = {
    val base64 = extractPemSection(pem, "PRIVATE KEY")
    val keyBytes = Base64.getDecoder.decode(base64)
    val spec = new PKCS8EncodedKeySpec(keyBytes)
    KeyFactory.getInstance("RSA").generatePrivate(spec)
  }

  private def extractCertificate(pem: String): X509Certificate = {
    val base64 = extractPemSection(pem, "CERTIFICATE")
    val certBytes = Base64.getDecoder.decode(base64)
    val cf = CertificateFactory.getInstance("X.509")
    cf.generateCertificate(new ByteArrayInputStream(certBytes)).asInstanceOf[X509Certificate]
  }

  private def extractAllCertificates(pem: String): Seq[X509Certificate] = {
    val cf = CertificateFactory.getInstance("X.509")
    val beginMarker = "-----BEGIN CERTIFICATE-----"
    val endMarker = "-----END CERTIFICATE-----"
    val certs = scala.collection.mutable.ArrayBuffer[X509Certificate]()
    var searchFrom = 0
    while {
      val beginIdx = pem.indexOf(beginMarker, searchFrom)
      val endIdx = pem.indexOf(endMarker, searchFrom)
      if (beginIdx >= 0 && endIdx >= 0) {
        val base64 = pem.substring(beginIdx + beginMarker.length, endIdx).replaceAll("\\s", "")
        val certBytes = Base64.getDecoder.decode(base64)
        certs += cf.generateCertificate(new ByteArrayInputStream(certBytes)).asInstanceOf[X509Certificate]
        searchFrom = endIdx + endMarker.length
        true
      } else {
        false
      }
    } do ()
    certs.toSeq
  }

  private def extractPemSection(pem: String, sectionType: String): String = {
    val beginMarker = s"-----BEGIN $sectionType-----"
    val endMarker = s"-----END $sectionType-----"
    val beginIdx = pem.indexOf(beginMarker)
    val endIdx = pem.indexOf(endMarker)
    if (beginIdx < 0 || endIdx < 0) {
      throw new IllegalArgumentException(s"PEM section '$sectionType' not found")
    }
    pem.substring(beginIdx + beginMarker.length, endIdx)
      .replaceAll("\\s", "")
  }
}
