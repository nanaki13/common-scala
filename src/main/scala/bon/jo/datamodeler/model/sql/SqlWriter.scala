package bon.jo.datamodeler.model.sql


import bon.jo.datamodeler.model.sql.SqlWriter.{/,  writer}
import bon.jo.datamodeler.model.macros.SqlMacro
import bon.jo.datamodeler.model.macros.GenMacro
import bon.jo.datamodeler.util.ConnectionPool
import bon.jo.datamodeler.util.Pool
import bon.jo.datamodeler.util.Utils.UsingSb
import java.sql.Connection
import java.time.LocalDateTime


object SqlWriter:


 // type UsingSb[A] = StringBuilder ?=> A
  inline def writer: UsingSb[StringBuilder] = summon
  inline def /(s : Any): UsingSb[Unit] =  writer.append(s)
  case class SelectCase(value: String)(using StringBuilder):

    /("SELECT ")
    /(value)

    def fromCase(f: FromCase): UsingSb[SelectCase] =
      /(" FROM ")
      /(f.value)
      this



    def whereCase(w: Where): UsingSb[SelectCase] =
      /(" WHERE ")
      /(w.value)
      this

  inline def from[T]: UsingSb[Unit] =
    /(" FROM ")
    /(SqlMacro.tableName[T].name)
  inline def where[T](inline f: T => Any): UsingSb[Unit] =
    /(" WHERE ")
    /(GenMacro.fieldSelection[T](f)._2)

  case class FromCase(value: Any)
  case class Where(value: Any)

  inline def createTable[T]: UsingSb[Unit] =
    /("SELECT ")
    /(SqlMacro.columnsName[T])
  inline def select[T]: UsingSb[Unit] =
    /("SELECT ")
    /(SqlMacro.columnsName[T])

  inline def value[T]: UsingSb[Unit] =
    /(" VALUES (")
    /("?," * (SqlMacro.columnsCountInsert[T] - 1))
    /('?' )
    /(')')
  inline def insert[T]: UsingSb[Unit] =
    /("INSERT INTO ")
    /(SqlMacro.tableName[T].name)
    /('(')
    /(SqlMacro.columnsNameInsert[T])
    /(')')

  inline def update[T]: UsingSb[Unit] =
    /("UPDATE ")
    /(SqlMacro.tableName[T].name)
    /(" SET ")
    /(SqlMacro.columnsNameList[T].map(c => s"$c = ?").mkString(", "))
  inline def delete[T]: UsingSb[Unit] =
    /("DELETE FROM ")
    /(SqlMacro.tableName[T].name)

  inline def select[T,C]: UsingSb[Unit] =
    /("SELECT ")
    /(SqlMacro.columnsName[T])
    /(SqlMacro.columnsName[C])






  
  





