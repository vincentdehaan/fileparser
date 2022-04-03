package nl.vindh.fileparser

import atto.Atto.{anyChar, manyN}
import atto.Parser

trait ParseUtils {
  def uint(n: Int): Parser[Int] =
    manyN(n, anyChar).map(cs => cs.foldRight(0){
      (nw, acc) => acc * 256 + nw.toInt
    })
}
