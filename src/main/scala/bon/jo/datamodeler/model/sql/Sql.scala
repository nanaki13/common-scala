package bon.jo.datamodeler.model.sql
import bon.jo.datamodeler.model.sql.SqlWriter.{/,  writer}
import bon.jo.datamodeler.util.Utils.{-,UsingSb}
import bon.jo.datamodeler.model.macros.GenMacro
import bon.jo.datamodeler.model.macros.SqlMacro
//import Sql.SqFun
object Sql:

  inline def sql[E] :  -[Sql[E]] = summon
  class SqlImpl[A] extends Sql[A]
  inline def apply[A]():Sql[A] = SqlImpl()


trait Sql[A]:
  inline def count : UsingSb[Sql[A]] =
    SqlWriter.count[A]
    this
  inline def update : UsingSb[Sql[A]] =
    SqlWriter.update[A]
    this
  inline def delete : UsingSb[Sql[A]] =
    SqlWriter.delete[A]
    this
  inline def selectMe : UsingSb[Sql[A]] =
    SqlWriter.select[A]
    this
  inline def max(inline f: A => Any) : UsingSb[Sql[A]] =
    /(s"SELECT MAX(${GenMacro.fieldSelection[A](f)._2}) ")
    SqlWriter.from[A]
    this
  inline def maxId : UsingSb[Sql[A]] =
    /(s"SELECT MAX(${SqlMacro.uniqueIdString[A]}) ")
    SqlWriter.from[A]
    this
  inline def sqlFunction(sqlFun : String, inline f: A => Any) : UsingSb[Sql[A]] =
    /(s"SELECT $sqlFun(${GenMacro.fieldSelection[A](f)._2}) ")
    SqlWriter.from[A]
    this
  
  inline def sqlFunction(sqlFun : SqFun, inline f: A => Any) : UsingSb[Sql[A]] =
    /(s"SELECT $sqlFun(${GenMacro.fieldSelection[A](f)._2}) ")
    SqlWriter.from[A]
    this
  inline def select[B] : UsingSb[Sql[A]] =
    /(',')
    /(SqlMacro.columnsName[B])
    this

  inline def columnName[B] : UsingSb[Sql[A]] =
    /(SqlMacro.columnsName[B].mkString(", "))
    this

  inline def columnNameAlias[B](alias : String) : UsingSb[Sql[A]] =
    /(SqlMacro.columnsName[B].map(alias+"."+_).mkString(", "))
    this
  inline def from : UsingSb[Sql[A]] =
    SqlWriter.from[A]
    this

  inline def fromAlias(alias : String) : UsingSb[Sql[A]] =
    SqlWriter.from[A]
    /(" "+alias)
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
  inline def idClause :  UsingSb[Sql[A]] =
    /(SqlMacro.idsString[A].map(e => s"$e = ?").mkString(" AND "))
    this

  inline def or(inline f: A => Any) : UsingSb[Sql[A]] =
    /(" OR ")
    /(GenMacro.fieldSelection[A](f)._2)
    this

  inline def columnName(inline f: A => Any) : UsingSb[Sql[A]] =
    /(GenMacro.fieldSelection[A](f)._2)
    this
  inline def and(inline f: A => Any) : UsingSb[Sql[A]] =
    /(" AND ")
    /(GenMacro.fieldSelection[A](f)._2)
    this

  inline def join[B](aliasA : String, aliasB : String)(inline f: A => Any,inline g: B => Any): UsingSb[Sql[A]] =
    /(s" JOIN $aliasB.${SqlMacro.tableName[B].name}")
    /(" ON ")
    /(s"$aliasA.${GenMacro.fieldSelection[A](f)._2}")
    /(" = ")
    /(s"$aliasB.${GenMacro.fieldSelection[B](g)._2}")
    this


  def ===(b: Any): UsingSb[Sql[A]] =
    /(s" = $b")
    this
