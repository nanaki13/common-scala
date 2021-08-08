package bon.jo.datamodeler.model.`macro`

import scala.annotation.tailrec
import scala.quoted.{Expr, Quotes, ToExpr, Type, quotes}

object TestMacro:

  def inspectCode(x: Expr[Any])(using Quotes): Expr[Any] =
    Expr(x.show)

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



  inline def invlaidLambda(s : String = "only statments ending with select accepted") = throw IllegalStateException(s)
  def fieldSelectionCode[T : Type]( t : Expr[T])(using Quotes):Expr[(String,String)] =
    import quotes.reflect.*
    val tree : Tree = t.asTerm
    @tailrec def findFirstBlock(lTree : Tree) : Block=
      lTree match
        case Inlined(_,_,b@ Block(_,_)) =>  b
        case Inlined(_,_,b@ Inlined(_,_,_)) => findFirstBlock(b)
        case _ => invlaidLambda(s"findFirstBlock match error : $lTree")

    val bl = findFirstBlock(tree)
    bl match
      case  Block(statments,term) =>
        statments.last match {
          case DefDef(_,_,_,Some(Select(Ident(a),name))) => Expr((a,name))
          case _ => println("Local");println(statments.last);invlaidLambda()
        }
      case _ =>
        println("Global");println(tree);invlaidLambda()








