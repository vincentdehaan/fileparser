package nl.vindh.fileparser

import java.nio.file.{Files, Paths}

object FileParser {
  def main(args: Array[String]): Unit = {
    val bytes = Files.readAllBytes(Paths.get(args(0)))
    val chars = bytes.map(b => (b.toInt & 0xFF).toChar).mkString


    val parsed = BMPFile.parse(chars)

    println(parsed)
  }

}
