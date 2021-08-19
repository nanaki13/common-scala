package bon.jo.datamodeler.model.macros

import scala.annotation.tailrec
import scala.quoted.{Expr, Quotes, ToExpr, Type, quotes}
import java.sql.PreparedStatement
import java.time.LocalDate
import bon.jo.datamodeler.util.Utils.{/,writer,UsingSb}
object SqlMacro:

  case class Table(name: String)

  
  inline def columnsName[T]: String = ${ columnsNameCode[T]() }
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
    //  println(tpe.typeSymbol.declaredFields  )
    // println(tpe.typeSymbol.declaredMethods  )

    val l: List[Symbol] = tpe.typeSymbol.declaredFields
    /* l.foreach(s =>
      tpe.memberType(s).asType match
        case '[Int]    => println(s.name + " it's an int ")
        case '[Tuple]  => println(s.name + " it's an tuple ")
        case '[String] => println(s.name + " it's a string ")
        case a @ _     => println(s.name + " it's not an int : " + tpe.memberType(s).show)
    )*/
    Expr(l.map(_.name).mkString(","))



  inline def sqlTypesDef[T]:List[String] = 
    ${sqlTypesDefCode[T]}



  def sqlTypesDefCode[T : Type](using  Quotes):Expr[List[String]] =
    import quotes.reflect.*
    import bon.jo.datamodeler.model.SimpleSql.id
    val tpe: TypeRepr = TypeRepr.of[T]
    val sbe: Symbol = TypeRepr.of[id].typeSymbol
    val symbol = tpe.typeSymbol
    val fields = symbol.caseFields


    val idFieldsSymbols = symbol.primaryConstructor.paramSymss.flatMap(_.filter(_.getAnnotation(sbe).nonEmpty))
    val f = fields.map(f => 
        given StringBuilder = StringBuilder()
        val isId = idFieldsSymbols.map(_.name == f.name).reduce(_ || _) || f.name == "id"

        /( s"${f.name}")
        tpe.memberType(f).asType match 
          case '[Int] =>  /(" INT")
          case '[Long] =>  /(" BIGINT")
          case '[String] =>  /(" VARCHAR(255)")
          case '[Float] =>  /(" FLOAT")
          case '[Double] =>  /(" DOUBLE")
          case '[LocalDate] =>  /(" DATE")
        if f.name == "id" then /(" PRIMARY KEY") 
        writer.toString
      )
    Expr(f)
  

  inline def testCreateFunction[T]:T => String =
      ${createFunction[T]}
  def createFunction[A: Type](using  Quotes): Expr[A => String] =
      '{(a: A) => ${Help2().createFunctionBody('{a})}}
  inline def fillInsert[T]:(T,PreparedStatement) => Unit =
      ${fillInsertCode[T]}
  def fillInsertCode[A: Type](using  Quotes): Expr[(A,PreparedStatement) => Unit] =
      '{(a: A,p : PreparedStatement) => ${Help2().fillInsertBody('{a},'{p})}}

  class Help2[Q <: Quotes, T : Type]()(using val qq : Q) :
    import  qq.reflect.*
    def createFunctionBody[A](param: Expr[A]): Expr[String] = 
      val fields = TypeTree.of[T].symbol.caseFields
      val accessors : List[Expr[_]] = fields.flatMap(f => List(Expr(f.name),Select(param.asTerm, f).asExpr))
      val l = Expr.ofList(accessors)
      '{
        ${l}.mkString(",")
       }
    def fillInsertBody[A](param: Expr[A],stmt: Expr[PreparedStatement]): Expr[Unit] = 
      val fields = TypeTree.of[T].symbol.caseFields
      val accessors : List[Expr[_]] = fields.map(f => Select(param.asTerm, f).asExpr)
      val l = Expr.ofList(accessors)
      '{
        ${l}.zipWithIndex.map{ (e,i) =>
          ${stmt}.setObject(i+1,e)
        }
        ()  
       }
   
  









