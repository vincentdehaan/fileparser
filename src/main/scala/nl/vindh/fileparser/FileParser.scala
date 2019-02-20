package nl.vindh.fileparser

import scala.io.Source

object FileParser {
  def main(args: Array[String]): Unit = {
    val chars = Source.fromFile(args(0)).toList

    val parsed = parse(chars)

    println(parsed)
  }

  def parse(chars: List[Char]): File = {
    chars match {
      case f: BMPFile => f
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