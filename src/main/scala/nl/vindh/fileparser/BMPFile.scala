package nl.vindh.fileparser

import atto._
import Atto._
import atto.ParseResult.Done

object BMPFile {
  def parse(s: String) = {
    bmpParser.parse(s).done match {
      case Done(x, y) => y // TODO
    }

  }

  val bmpSignatureParser: Parser[BMPSignature] =
    manyN(2, letter).filter {
      chars => BMPSignature.signatures.contains(chars.mkString)
    }.map {
      _.mkString match {
        case "BM" => BMSignature
        case "BA" => BASignature
        case "CI" => CISignature
        case "CP" => CPSignature
        case "IC" => ICSignature
        case "PT" => PTSignature // TODO: how would we handle a theoretical case _ => (which sould not occur due to the filter)
      }
    }.namedOpaque("BMPSignature")

  // TODO: make this available for all parsers
  def uint(n: Int): Parser[Int] =
    manyN(n, anyChar).map(cs => cs.foldRight(0){
      (nw, acc) => acc * 256 + nw.toInt
  })

  val bmpHeaderParser: Parser[BMPHeader] =
    (for {
      sig <- bmpSignatureParser
      size <- uint(4).namedOpaque("file size")
      re21 <- uint(2).namedOpaque("reserved 1")
      re22 <- uint(2).namedOpaque("reserved 2")
      offset <- uint(4).namedOpaque("offset")
    } yield BMPHeader(sig, size, re21, re22, offset)).named("BMPHeader")

  val dibHeaderParser: Parser[DibHeader] =
    uint(4).namedOpaque("header size").flatMap {
      case 12 => bmpCoreHeaderParser
      case 40 => bmpInfoHeaderParser // TODO: add other header types; see Wikipedia
    }


  val bmpCoreHeaderParser: Parser[DibHeader] = // TODO: test against real-world files
    (for {
      w <- uint(2).namedOpaque("width")
      h <- uint(2).namedOpaque("height")
      _ <- uint(2).filter(_ == 1).namedOpaque("color planes") // TODO: implement withFilter on Parser; if colorPlanes == 1
      bits <- uint(2).namedOpaque("bits")
    } yield BMPCoreHeader(w, h, bits))
      .named("BMPCoreHeader")
      .map(_.asInstanceOf[DibHeader]) // TODO: why is Parser not covariant?

  val bmpInfoHeaderParser: Parser[DibHeader] =
    (for {
      width <- uint(4).namedOpaque("width")
      height <- uint(4).namedOpaque("height")
      _ <- uint(2).filter(_ == 1).namedOpaque("color planes")
      bits <- uint(2).namedOpaque("bits")
      cm <- uint(4).namedOpaque("compression method")
      size <- uint(4).namedOpaque("image size")
      hres <- uint(4).namedOpaque("horizontal resolution")
      vres <- uint(4).namedOpaque("vertical resolution")
      colors <- uint(4).namedOpaque("colors")
      important <- uint(4).namedOpaque("important colors")
    } yield BMPInfoHeader(width, height, bits, cm, size, hres, vres, colors, important))
      .named("BMPInfoHeader")
      .map(_.asInstanceOf[DibHeader]) // TODO: why is Parser not covariant?

  val bmpParser: Parser[BMPFile] =
    (for {
      header <- bmpHeaderParser
      dibHeader <- dibHeaderParser
    } yield BMPFile(header, dibHeader)).named("BMPFile")
}

case class BMPFile(header: BMPHeader, dibHeader: DibHeader)

// TODO: add this to some namespace
trait BMPSignature extends Product with Serializable // TODO: why is this necessary to make bmpSignatureParser type-check?
case object BMSignature extends BMPSignature
case object BASignature extends BMPSignature
case object CISignature extends BMPSignature
case object CPSignature extends BMPSignature
case object ICSignature extends BMPSignature
case object PTSignature extends BMPSignature
object BMPSignature {
  val signatures = List("BM", "BA", "CI", "CP", "IC", "PT") // TODO: get rid of code duplication
}

case class BMPHeader(signature: BMPSignature, fileSize: Int, reserved1: Int, reserved2: Int, fileOffset: Int)

trait DibHeader

case class BMPCoreHeader(width: Int, height: Int, bits: Int) extends DibHeader
case class BMPInfoHeader(width: Int, height: Int, bits: Int, compressionMethod: Int /* TODO: refine */, imageSize: Int, hRes: Int, vres: Int, colors: Int, importantColors: Int) extends DibHeader