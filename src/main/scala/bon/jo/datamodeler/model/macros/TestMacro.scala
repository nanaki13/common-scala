package bon.jo.datamodeler.model.macros

import scala.annotation.tailrec
import scala.quoted.{Expr, Quotes, ToExpr, Type, quotes}
import java.sql.PreparedStatement
import java.time.LocalDate

object TestMacro:

  def inspectCode(x: Expr[Any])(using Quotes): Expr[Any] =
    Expr(x.show)

  inline def countFields[T]: Int = ${ countFields[T]() }
  inline def columnsName[T]: String = ${ columnsNameCode[T]() }
  inline def tableName[T]: Table = ${ tableNameCode[T]() }
  inline def where[T](inline f: T => Any): String = ${ whereCode[T]('f) }
  case class Table(name: String)

  given ToExpr[Table] with {
    def apply(x: Table)(using Quotes): Expr[Table] = '{ Table(${ Expr(x.name) }) }

  }
  given ToExpr[Unit] with {
    def apply(x: Unit)(using Quotes): Expr[Unit] = '{  }
  }
  def countFields[T: Type]()(using Quotes): Expr[Int] =
    import quotes.reflect.*
    val tpe: TypeRepr = TypeRepr.of[T]
    Expr(tpe.typeSymbol.declaredFields.size)
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

  inline def fieldSelection[T](inline t : T => Any):(String,String) =
    ${fieldSelectionCode[T => Any]('t)}

  inline def sqlTypesDef[T]:List[String] = 
    ${sqlTypesDefCode[T]}

  private type UsingSb[A] = StringBuilder ?=> A

  private inline def writer: UsingSb[StringBuilder] = summon

  private inline def /(s : Any): UsingSb[Unit] =  writer.append(s)

  def sqlTypesDefCode[T : Type](using  Quotes):Expr[List[String]] =
    import quotes.reflect.*
    val tpe: TypeRepr = TypeRepr.of[T]
    val fields = tpe.typeSymbol.caseFields
    val f = fields.map(f => 
        given StringBuilder = StringBuilder()
        /( s"${f.name}")
        tpe.memberType(f).asType match 
          case '[Int] =>  /(" INT")
          case '[Long] =>  /(" BIGINT")
          case '[String] =>  /(" VARCHAR(255)")
          case '[Float] =>  /(" FLOAT")
          case '[Double] =>  /(" DOUBLE")
          case '[LocalDate] =>  /(" DATE")
        writer.toString
      )
    Expr(f)
  def fieldSelectionCode[T : Type]( t : Expr[T])(using  Quotes):Expr[(String,String)] = Help().fieldSelectionCode(t)

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
   
  
  class Help[Q <: Quotes, T : Type]()(using val qq : Q) :
 
    import  qq.reflect.*
    val tpe = TypeRepr.of[T]
    val symbol = tpe.typeSymbol

    def name = symbol.name


    def fieldSelectionCode[T : Type]( t : Expr[T]):Expr[(String,String)] =
     
      val tree : Tree = t.asTerm
     

      val bl = findFirstBlock(tree)
      bl match
        case  Block(statments,term) =>
          statments.last match {
            case DefDef(_,_,_,Some(Select(Ident(a),name))) => Expr((a,name))
            case _ => println("Local");println(statments.last);invlaidLambda()
          }
        case _ =>
          println("Global");println(tree);invlaidLambda()

    end fieldSelectionCode

    @tailrec 
    final def findFirstBlock(lTree : Tree) : Block=
      lTree match
        case Inlined(_,_,b@ Block(_,_)) =>  b
        case Inlined(_,_,b@ Inlined(_,_,_)) => findFirstBlock(b)
        case _ => invlaidLambda(s"findFirstBlock match error : $lTree")
    end findFirstBlock
    

          

inline def invlaidLambda(s : String = "only statments ending with select accepted") = throw IllegalStateException(s)

object PrintTree {
  inline def printTree[T](inline x: T): Unit = ${printTreeImpl('x)}
  def printTreeImpl[T: Type](x: Expr[T])(using qctx: Quotes): Expr[Unit] =
    import qctx.reflect.*
    println(x.asTerm.show(using Printer.TreeStructure))
    println(x.asTerm)
    println(x.show)
    '{()}
}









