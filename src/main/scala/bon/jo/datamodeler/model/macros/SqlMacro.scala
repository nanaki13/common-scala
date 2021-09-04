package bon.jo.datamodeler.model.macros

import scala.annotation.tailrec
import scala.quoted.{Expr, Quotes, ToExpr, Type, quotes}
import java.sql.PreparedStatement
import java.sql.ResultSet
import java.time.LocalDate
import bon.jo.datamodeler.util.Utils.{/,writer,UsingSb}

import scala.collection.mutable.ListBuffer
import java.time.LocalDateTime
object SqlMacro:

  case class Table(name: String)

  
  inline def columnsName[T]: String = ${ columnsNameCode[T]() }
  inline def columnsNameList[T]: List[String] = ${ columnsNameListCode[T]() }
  inline def tableName[T]: Table = ${ tableNameCode[T]() }
  inline def where[T](inline f: T => Any): String = ${ whereCode[T]('f) }



  given ToExpr[Table] with {
    def apply(x: Table)(using Quotes): Expr[Table] = '{ Table(${ Expr(x.name) }) }

  }
  given ToExpr[Unit] with {
    def apply(x: Unit)(using Quotes): Expr[Unit] = '{  }
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


  def columnsNameListCode[T: Type]()(using Quotes): Expr[List[String]] =
    import quotes.reflect.*
    val tpe: TypeRepr = TypeRepr.of[T]
    val l: List[Symbol] = tpe.typeSymbol.declaredFields
    Expr(l.map(_.name))

  inline def sqlTypesDef[T]:List[String] = 
    ${sqlTypesDefCode[T]}
  inline def uniqueIdValue[T,ID]:T => ID = uniqueIdValueAny[T].andThen(_.asInstanceOf[ID])
  inline def uniqueIdValueAny[T]:(e : T) => Any = ${uniqueIdValueCode[T]}
  def uniqueIdValueCode[T: Type](using  Quotes) :Expr[T => Any] = '{
    (t : T) =>  ${SqlMacroHelper().uniqueIdValueCode('t)}
  }
  def uniqueIdStringCode[T: Type](using  Quotes):Expr[String] = 
    SqlMacroHelper().uniqueIdString

    
  inline def uniqueIdString[T]:String = 
    ${uniqueIdStringCode[T]}
  inline def idsString[T]:List[String] = 
    ${idStringCode[T]}

  def idStringCode[T: Type](using  Quotes):Expr[List[String]] = 
    SqlMacroHelper().idString

  




  def sqlTypesDefCode[T : Type](using  Quotes):Expr[List[String]] =
    import quotes.reflect.*
    import bon.jo.datamodeler.model.sql.SimpleSql.id
    val tpe: TypeRepr = TypeRepr.of[T]
    val sbe: Symbol = TypeRepr.of[id].typeSymbol
    val symbol = tpe.typeSymbol
    val fields = symbol.caseFields


   // val idFieldsSymbols = symbol.primaryConstructor.paramSymss.flatMap(_.filter(_.getAnnotation(sbe).nonEmpty))
    val f = fields.map(f => 
        given StringBuilder = StringBuilder()
        val isId = SqlMacroHelper[Quotes,T]().idFieldsCode

        /( s"${f.name}")
        tpe.memberType(f).asType match 
          case '[Int] =>  /(" INT")
          case '[Long] =>  /(" BIGINT")
          case '[String] =>  /(" VARCHAR(255)")
          case '[Float] =>  /(" FLOAT")
          case '[Double] =>  /(" DOUBLE")
          case '[LocalDate] =>  /(" DATE")
          case '[LocalDateTime] =>  /(" DATETIME")
        if f.name == "id" then /(" PRIMARY KEY") 
        writer.toString
      )
    Expr(f)
  

  inline def testCreateFunction[T]:T => String =
      ${createFunction[T]}
  def createFunction[A: Type](using  Quotes): Expr[A => String] =
      '{(a: A) => ${SqlMacroHelper().createFunctionBody('{a})}}
  inline def fillInsert[T]:(T,PreparedStatement) => Unit =
      ${fillInsertCode[T]}
  def fillInsertCode[A: Type](using  Quotes): Expr[(A,PreparedStatement) => Unit] =
      '{(a: A,p : PreparedStatement) => ${SqlMacroHelper().fillInsertBody('{a},'{p})}}
  
  def readResultCode[T: Type](using  Quotes): Expr[(ResultSet,Int ) => Seq[Any]] =
      '{( r : ResultSet,offset : Int) => ${SqlMacroHelper().readResultBody('{r},'{offset})}}
  
  inline def readResultSet[T]:( r : ResultSet,offset : Int) => Seq[Any] =
    ${readResultCode[T]}
  
  
  









