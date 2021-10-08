package bon.jo.datamodeler.model.macros

import bon.jo.datamodeler.model.macros.SqlMacro.Table
import bon.jo.datamodeler.model.macros.{FieldInfo, FieldsClass}

import java.text.SimpleDateFormat
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.annotation.tailrec
import scala.quoted.{Expr, Quotes, ToExpr, Type}


class MacroHelper[Q <: Quotes, T : Type]()(using val qq : Q) :

  import  qq.reflect.*
  lazy val tpe : TypeRepr = TypeRepr.of[T]
  lazy val symbol = tpe.typeSymbol
  lazy val tr = TypeTree.of[T]
  lazy val constructor = symbol.primaryConstructor
  lazy val  name = symbol.name

  lazy val constructorParamLists: List[List[Symbol]] = constructor.paramSymss
  lazy val fields : List[qq.reflect.Symbol] = symbol.caseFields


  def fieldsClass : Expr[FieldsClass] =

    val pFun = Expr.ofList( fields.map{s =>
      val tpeField: TypeRepr = tpe.memberType(s)
      val parsefun  = tpeField.asType match
        case '[String] => '{(sd  : String) => sd}
        case '[Int] => '{(sd  : String) => sd.toInt }
        case '[List[String]] => '{(sd  : String) => List(sd.split(";")) }
        case '[LocalDateTime] => '{ (sd  : String) => LocalDateTime.parse(sd,DateTimeFormatter.ISO_LOCAL_DATE_TIME) }
        case '[java.util.Date] => '{ (sd  : String) => new SimpleDateFormat("yyyy-MM-ddTHH:mm:ss").parse(sd) }
        case _ => throw new UnsupportedOperationException(s.name+ " :unhadle : "+tpeField.show)


        '{FieldInfo(${Expr(s.name)},${Expr(tpeField.show)},$parsefun )}
    })
    '{FieldsClass(${Expr(tpe.show)}, $pFun)}

  def listTo[E : Type](listExp : Expr[Seq[Any]]) : Expr[E] =
    val size = tpe.typeSymbol.declaredFields.size
    val listSymbol = TypeRepr.of[scala.collection.SeqOps].typeSymbol
    val applyMethod =listSymbol.declaredMethods.find(_.name == "apply")
    val typeParmes = constructorParamLists(0).map(tpe.memberType(_))

    val parms = for (i <- 0 until size)
      yield(tp : TypeRepr) => TypeApply(Select.unique(Apply(Select(listExp.asTerm, applyMethod.get), List(Literal(IntConstant(i)))), "asInstanceOf"), List(Inferred(tp)))
    val typed = (constructorParamLists(0).map(tpe.memberType(_)) zip parms) map ((tp,fTp) => fTp(tp))
    Inlined(None, Nil, Apply(Select(New(TypeIdent(symbol)), constructor), typed)).asExprOf[E]


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


