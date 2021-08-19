package bon.jo.datamodeler.util

object Utils:
  type UsingSb[A] = StringBuilder ?=> A

  inline def writer: UsingSb[StringBuilder] = summon

  inline def /(s : Any): UsingSb[Unit] =  writer.append(s)