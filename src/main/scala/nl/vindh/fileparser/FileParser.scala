package nl.vindh.fileparser

import java.io.File
import java.nio.file.{Files, Paths}

import atto._
import Atto._
import atto.ParseResult.Done
import com.monovore.decline._
import cats.implicits._

object FileParser extends CommandApp(
  name = "file-parser",
  header = "File parser",
  main = {
    import FileParserHelper._

    val recursiveFlag = Opts.flag("recursive", "recursive search; last argument should be a folder", "r").orFalse
    val parserOpt = Opts.option[String]("parser", "select the parser", "p").orElse(Opts.apply("")) // TODO: is this what I want?
    val extensionOpt = Opts.option[String]("extension", "parse only files with this extension", "e").orElse(Opts.apply("")) // TODO: idem
    val targetArg = Opts.argument[String]()

    (recursiveFlag, parserOpt, extensionOpt, targetArg).mapN {
      (recursive, parserId, extension, target) => {
        println(s"recursive: $recursive")
        println(s"|$extension|")
        val files = if (recursive) filesInFolder(new File(target)) else List(new File(target))
        println(s"files found: ${files.size}")

        files.filter(_.getName.endsWith(extension)).map {
          file => {
            parsers.get(parserId) match {
              case None => println(s"Unknown parser: $parserId")
              case Some(parser) => {
                val bytes = Files.readAllBytes(Paths.get(file.getPath))
                val chars = bytes.map(b => (b.toInt & 0xFF).toChar).mkString

                parser.parse(chars).done match {
                  case Done(x, y) => println(y)
                  case x => println(s"VINCENTS PARSING ERROR $x")
                }
              }
            }
          }
        }
      }
    }
  })

object FileParserHelper {
  // TODO: create iterator or stream or other lazy mechanism
  def filesInFolder(file: File): List[File] =
    if(file.exists && file.canRead)
      if(file.isDirectory)
        file.listFiles.flatMap(filesInFolder).toList
      else
        List(file)
    else
      Nil

  val parsers = Map(
    "bmp" -> BMPFile.bmpParser,
    "elf" -> ELFFile.elfParser)
}
