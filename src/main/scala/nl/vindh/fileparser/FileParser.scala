package nl.vindh.fileparser

import java.nio.file.{Files, Paths}

object FileParser {
  def main(args: Array[String]): Unit = {
    val chars = Files.readAllBytes(Paths.get(args(0))).toList

    val parsed = parse(chars)

    println(parsed)
  }

  def parse(chars: List[Byte]): File = {
    chars match {
      case BMPFileExtractor(f) => f
      case _ => new File {}
    }
  }
}

trait File

trait FileSegment

object XXX extends FileSegment

object :=> {
  //def unapply(chars: List[Char]): 
}