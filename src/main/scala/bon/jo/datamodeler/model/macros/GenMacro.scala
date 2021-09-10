package bon.jo.datamodeler.model.macros
import scala.quoted.{Expr, Quotes, ToExpr, Type, quotes}
import scala.annotation.tailrec
import java.sql.ResultSet
object GenMacro :
  def inspectCode(x: Expr[Any])(using Quotes): Expr[Any] =
    Expr(x.show)

  inline def log(inline x: Any): Unit =
    println(debug(x))
  def debugCode(x: Expr[Any])(using Quotes): Expr[Any] =
   '{${ Expr(x.show)} + " = "+ ${x}}
  inline def debug(inline x: Any): Any =
    ${debugCode('x)}
  inline def countFields[T]: Int = ${ countFields[T]() }

  def countFields[T: Type]()(using Quotes): Expr[Int] =
    import quotes.reflect.*
    val tpe: TypeRepr = TypeRepr.of[T]
    Expr(tpe.typeSymbol.declaredFields.size)

  /*inline def testConstructor[T] :T= ${ testConstructorCode[T] }

  def testConstructorCode[T : Type ](using  Quotes) : Expr[T]= 
    import quotes.reflect.*
    val tpe = TypeRepr.of[T]
    val tr = TypeTree.of[T]
    val symbol = tpe.typeSymbol
    val construcot = symbol.primaryConstructor
    val x = Inlined(None, Nil, Apply(Select(New(TypeIdent(symbol)), construcot), List(Literal(IntConstant(0)), Literal(StringConstant("abc")), Literal(IntConstant(1)), Literal(StringConstant("email")))))  
    x.asExprOf[T]*/


  inline def printTree[T](inline x: T): Unit = ${printTreeImpl('x)}
  def printTreeImpl[T: Type](x: Expr[T])(using qctx: Quotes): Expr[Unit] =
    import qctx.reflect.*
    println(x.asTerm.show(using Printer.TreeStructure))
    println(x.asTerm)
    println(x.show)


    '{()}


  inline def fieldSelection[T](inline t : T => Any):(String,String) =
    ${fieldSelectionCode[T => Any]('t)}
  def fieldSelectionCode[T : Type]( t : Expr[T])(using  Quotes):Expr[(String,String)] =
    Help().fieldSelectionCode(t)

  inline def listTo[T](inline t : Seq[Any]):T =
      ${listToCode[T]('t)}

  inline def listToFunction[T]:Seq[Any] => T =
      ${listToFunctionCode[T]}

  def listToCode[T : Type]( t : Expr[Seq[Any]])(using  Quotes):Expr[T] =
    Help().listTo(t)

  def listToFunctionCode[T : Type](using  Quotes):Expr[Seq[Any] =>T] =
    '{t => ${Help().listTo('t)}}
  



class Help[Q <: Quotes, T : Type]()(using val qq : Q) :
 
  import  qq.reflect.*
  lazy val tpe : TypeRepr = TypeRepr.of[T]
  lazy val symbol = tpe.typeSymbol
  lazy val tr = TypeTree.of[T]
  lazy val constructor = symbol.primaryConstructor
  lazy val  name = symbol.name

  lazy val constructorParamLists: List[List[Symbol]] = constructor.paramSymss

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


