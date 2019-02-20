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

object BMPFile {
  def unapply(chars: List[Char]): Option[BMPFile] = {
    (chars(0).toString + chars(1).toString) match {
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

case class BMPHeader(signature: String)