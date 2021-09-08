package bon.jo.datamodeler.util

import scala.annotation.tailrec

object Alias {

  lazy val alias : Alias = Alias()
  
  given Alias = alias

  inline def nextAlias(using Alias) = summon[Alias].next()

  val symbol = 'a' to 'z'
  @tailrec private def next(int: Int)(buff : Seq[Char]) : Seq[Char] =
    val d = int / symbol.size
    val r = int % symbol.size
    val s = symbol(r)
    val nextBuff = s  +: buff
    if d == 0
    then nextBuff
    else
      next(d)(nextBuff)


  def next(int: Int):String = next(int)(Nil).mkString
  @main def testAlis() = (0 to 100).map(next).foreach(println)
}

class Alias:
  var current = 0
  def next() : String =
    val ret = Alias.next(current)
    current+=1
    ret
