package bon.jo.datamodeler.util

import scala.collection.mutable.StringBuilder

object Utils:
  type UsingSb[A] = StringBuilder ?=> A

  inline def writer: UsingSb[StringBuilder] = summon

  inline def /(s : Any): UsingSb[Unit] =  writer.append(s)

  inline def stringBuilder[A](f : StringBuilder ?=> Unit):String = 
    given StringBuilder = StringBuilder()
    f
    writer.toString