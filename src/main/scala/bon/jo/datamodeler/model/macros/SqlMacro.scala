package bon.jo.datamodeler.model.macros

import scala.annotation.tailrec
import scala.quoted.{Expr, Quotes, ToExpr, Type, quotes}
import java.sql.PreparedStatement
import java.sql.ResultSet
import java.time.LocalDate
import bon.jo.datamodeler.util.Utils.{/, UsingSb, writer}
import bon.jo.datamodeler.model.macros.SqlMacroHelper
import bon.jo.datamodeler.model.sql.Filtre

import scala.collection.mutable.ListBuffer
import java.time.LocalDateTime
object SqlMacro:

  case class Table(name: String)

  inline def columnsCountInsert[T] : Int = ${ columnsCountInsert[T]() }
  inline def columnsName[T]: String = ${ columnsNameCode[T]() }
  inline def columnsNameInsert[T]: String = ${ columnsNameInsert[T]() }
  inline def columnsNameList[T]: List[String] = ${ columnsNameListCode[T]() }
  inline def tableName[T]: Table = ${ tableNameCode[T]() }
  inline def where[T](inline f: T => Any): String = ${ whereCode[T]('f) }
  inline def sqlTypesDef[T]: List[String] = ${ sqlTypesDefCode[T] }

  given ToExpr[Table] with {
    def apply(x: Table)(using Quotes): Expr[Table] = '{ Table(${ Expr(x.name) }) }

  }
  given ToExpr[Unit] with {
    def apply(x: Unit)(using Quotes): Expr[Unit] = '{}
  }

  def tableNameCode[T: Type]()(using Quotes): Expr[Table] =
    import quotes.reflect.*
    Expr(Table(TypeRepr.of[T].typeSymbol.name.toUpperCase))

  def whereCode[T: Type](f: Expr[T => Any])(using Quotes): Expr[String] =
    import quotes.reflect.*

    val s: String = f.show
    val ret = s.substring(s.lastIndexOf('.') + 1, s.lastIndexOf(')'))
    Expr(ret)
  def columnsNameCode[T: Type]()(using Quotes): Expr[String] =
    import quotes.reflect.*
    val tpe: TypeRepr = TypeRepr.of[T]
    val l: List[Symbol] = tpe.typeSymbol.declaredFields
    Expr(l.map(_.name).mkString(","))

  def columnsNameInsert[T: Type]()(using Quotes): Expr[String] =
    SqlMacroHelper().columnsNameCodeInsert
  def columnsCountInsert[T: Type]()(using Quotes): Expr[Int] =
    SqlMacroHelper().columnsCountInsert
  def columnsNameListCode[T: Type]()(using Quotes): Expr[List[String]] =
    import quotes.reflect.*
    val tpe: TypeRepr = TypeRepr.of[T]
    val l: List[Symbol] = tpe.typeSymbol.declaredFields
    Expr(l.map(_.name))

  def sqlTypesDefCode[T: Type](using Quotes): Expr[List[String]] =
    SqlMacroHelper().sqlTypesDefCode
  inline def uniqueIdValue[T, ID]: T => ID = uniqueIdValueAny[T].andThen(_.asInstanceOf[ID])
  inline def uniqueIdValueAny[T]: (e: T) => Any = ${ uniqueIdValueCode[T] }
  def uniqueIdValueCode[T: Type](using Quotes): Expr[T => Any] = '{ (t: T) =>
    ${ SqlMacroHelper().uniqueIdValueCode('t) }
  }
  def uniqueIdStringCode[T: Type](using Quotes): Expr[String] =
    SqlMacroHelper().uniqueIdString

  inline def uniqueIdString[T]: String =
    ${ uniqueIdStringCode[T] }
  inline def idsString[T]: List[String] =
    ${ idStringCode[T] }

  def idStringCode[T: Type](using Quotes): Expr[List[String]] =
    SqlMacroHelper().idString

  inline def testCreateFunction[T]: T => String =
    ${ createFunction[T] }
  def createFunction[A: Type](using Quotes): Expr[A => String] =
    '{ (a: A) => ${ SqlMacroHelper().createFunctionBody('{ a }) } }

  inline def isMonoIdAi[T]: Boolean =
    ${ isMonoIdAi[T] }
  def isMonoIdAi[T](using Quotes): Expr[Boolean] =
    SqlMacroHelper().isMonoIdAiExpr
  inline def fillInsert[T]: (T, PreparedStatement) => Unit =
    ${ fillInsertCode[T] }
  def fillInsertCode[A: Type](using Quotes): Expr[(A, PreparedStatement) => Unit] =
    '{ (a: A, p: PreparedStatement) => ${ SqlMacroHelper().fillInsertBody('{ a }, '{ p }) } }

  inline def fillUpdate[T]: (T, PreparedStatement) => Unit =
    ${ fillUpdateCode[T] }
  def fillUpdateCode[A: Type](using Quotes): Expr[(A, PreparedStatement) => Unit] =
    '{ (a: A, p: PreparedStatement) => ${ SqlMacroHelper().fillUpdateBody('{ a }, '{ p }) } }

  def readResultCode[T: Type](using Quotes): Expr[(ResultSet, Int) => Seq[Any]] =
    '{ (r: ResultSet, offset: Int) => ${ SqlMacroHelper().readResultBody('{ r }, '{ offset }) } }

  def readResultCodeTo[T: Type](using Quotes): Expr[(ResultSet, Int) => T] =
    '{ (r: ResultSet, offset: Int) => ${ SqlMacroHelper().readResultToBody('{ r }, '{ offset }) } }

  inline def readResultSet[T]: (r: ResultSet, offset: Int) => Seq[Any] =
    ${ readResultCode[T] }

  inline def readResultSetTo[T]: (r: ResultSet, offset: Int) => T =
    ${ readResultCodeTo[T] }

  inline def fillPreparedStatmentWithId[T]: (T, Offset, PreparedStatement) => Unit =
    ${ fillPreparedStatmentWithIdCode[T] }
  type Offset = Int
  def fillPreparedStatmentWithIdCode[T: Type](using
      Quotes
  ): Expr[(T, Offset, PreparedStatement) => Unit] =

    val idSize = SqlMacroHelper().idFieldsCode.size

    if (idSize == 1) then
      '{ (a: T, offset: Offset, p: PreparedStatement) =>
        ${ SqlMacroHelper().fillPreparedStatmentWithUniqueId('{ a }, '{ offset }, '{ p }) }
      }
    else if (idSize > 1) then
      '{ (a: T, offset: Offset, p: PreparedStatement) =>
        ${ SqlMacroHelper().fillPreparedStatmentWithId('{ a }, '{ offset }, '{ p }) }
      }
    else
      '{
        val table = ${ tableNameCode() }
        throw SqlMacroEx(s"no id in $table")
      }

  class SqlMacroEx(val message: String) extends Exception
