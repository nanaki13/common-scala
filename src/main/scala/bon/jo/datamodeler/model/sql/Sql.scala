package bon.jo.datamodeler.model.sql
import bon.jo.datamodeler.model.sql.SqlWriter.{/, UsingSb,  writer}
import bon.jo.datamodeler.model.macros.GenMacro
import bon.jo.datamodeler.model.macros.SqlMacro
object Sql:
  class SqlImpl[A] extends Sql[A]
  inline def apply[A]():Sql[A] = SqlImpl()

trait Sql[A]:
  inline def selectMe : UsingSb[Sql[A]] =
    SqlWriter.select[A]
    this
  inline def max(inline f: A => Any) : UsingSb[Sql[A]] =
    /(s"SELECT MAX(${GenMacro.fieldSelection[A](f)._2}) ")
    SqlWriter.from[A]
    this
  inline def select[B] : UsingSb[Sql[A]] =
    /(',')
    /(SqlMacro.columnsName[B])
    this
  inline def from : UsingSb[Sql[A]] =
    SqlWriter.from[A]
    this
  inline def insert : UsingSb[Sql[A]] =
    SqlWriter.insert[A]
    this
  inline def value : UsingSb[Sql[A]] =
    SqlWriter.value[A]
    this
  inline def where(inline f: A => Any) : UsingSb[Sql[A]] =
    SqlWriter.where[A](f)
    this

  inline def or(inline f: A => Any) : UsingSb[Sql[A]] =
    /(" OR ")
    /(GenMacro.fieldSelection[A](f)._2)
    this
  inline def and(inline f: A => Any) : UsingSb[Sql[A]] =
    /(" AND ")
    /(GenMacro.fieldSelection[A](f)._2)
    this

  inline def join[B](inline f: A => Any,inline g: B => Any): UsingSb[Sql[A]] =
    /(" JOIN ")
    /(SqlMacro.tableName[B].name)
    /(" ON ")
    /(GenMacro.fieldSelection[A](f)._2)
    /(" = ")
    /(GenMacro.fieldSelection[B](g)._2)
    this


  def ===(b: Any): UsingSb[Sql[A]] =
    /(s" = $b")
    this
