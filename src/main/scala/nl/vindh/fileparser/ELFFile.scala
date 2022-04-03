package nl.vindh.fileparser

import atto._
import Atto._
import atto.ParseResult.Done

object ELFFile extends ParseUtils {
  def parse(s: String) = {
// TODO: use abstraction
    elfParser.parse(s).done match {
      case Done(x, y) => y // TODO
      case x => println(s"VINCENTS PARSING ERROR $x")
    }
  }

  val elfHeader: Parser[ELFHeader] =
    (for {
      _ <- string(0x7f.toChar.toString + "ELF").namedOpaque("EI_MAG0-3")
      is64bits <- oneOf(s"${0x01.toChar.toString}${0x02.toChar.toString}").map(_ == 0x02.toChar).namedOpaque("EI_CLASS")
      littleEndian <- oneOf(s"${0x01.toChar.toString}${0x02.toChar.toString}").map(_ == 0x01.toChar).namedOpaque("EI_DATA")
      _ <- char(0x01).namedOpaque("EI_VERSION")
      targetOS <- anyChar.filter(_ < 0x12).map(_.toInt).namedOpaque("EI_OSABI")
      _ <- anyChar.namedOpaque("EI_ABIVERSION")
      _ <- manyN(7, anyChar).namedOpaque("EI_PAD")
      objectType <- uint(2).filter {
        case 0x00 | 0x01 | 0x02 | 0x03 | 0x04 | 0xfe00 | 0xfeff | 0xff00 | 0xffff => true
      }.namedOpaque("object type")
      targetArchitecture <- uint(2).namedOpaque("target architecture")
      entryAddress <- uint(if(is64bits) 8 else 4).namedOpaque("entry address") // TODO: if this number is very large, uint cannot create a real long number
    } yield ELFHeader(is64bits, littleEndian, targetOS, objectType, targetArchitecture, entryAddress)).named("ELFHeader")

  val elfParser: Parser[ELFFile] =
    (for {
      header <- elfHeader
    } yield ELFFile(header)).named("ELFFile")
}

case class ELFFile(header: ELFHeader)

// TODO: what is EI_ABIVERSION, EI_PAD?
// TODO: flags IMPORTANT
case class ELFHeader(
  is64bits: Boolean,
  littleEndian: Boolean,
  targetOS: Int /*TODO: refine type*/,
  objectType: Int /*TODO:refine type*/,
  targetArchitecture: Int /*TODO: refine type*/,
  entryAddress: Long)