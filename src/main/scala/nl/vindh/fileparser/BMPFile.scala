package nl.vindh.fileparser

case class BMPFile(
  bmpHeader: FileSegment,
  dibHeader: FileSegment,
  bitMasks: FileSegment,
  colorTable: FileSegment,
  gap1: FileSegment,
  pixelArray: FileSegment,
  gap2: FileSegment,
  iccColorProfile: FileSegment) extends File

object BMPFileExtractor {
  def unapply(bytes: List[Byte]): Option[BMPFile] = {
    (bytes(0).asInstanceOf[Char].toString + bytes(1).asInstanceOf[Char].toString) match {
      case "BM" => Some(
        BMPFile(
          bmpHeader = BMPHeader("BM"),
          dibHeader = XXX,
          bitMasks = XXX,
          colorTable = XXX,
          gap1 = XXX,
          pixelArray = XXX,
          gap2 = XXX,
          iccColorProfile = XXX))
      case _ => None
    }
  }
}

case class BMPHeader(signature: String) extends FileSegment