package bon.jo.datamodeler.model.macros
import scala.quoted.{Expr, Quotes, ToExpr, Type, quotes}
import java.sql.PreparedStatement
import java.sql.ResultSet
class SqlMacroHelper[Q <: Quotes, T : Type]()(using val qq : Q) :
    import  qq.reflect.*
    
    lazy val tpe: TypeRepr = TypeRepr.of[T]
    lazy val symbol = tpe.typeSymbol
    lazy val tree = TypeTree.of[T]
    lazy val fields : List[qq.reflect.Symbol] = symbol.caseFields
    def idFieldsCode : List[qq.reflect.Symbol] =
      import bon.jo.datamodeler.model.sql.SimpleSql.id
      lazy val annoId: Symbol = TypeRepr.of[id].typeSymbol
      symbol.primaryConstructor.paramSymss.flatMap(_.filter( s => s.name == "id" || s.getAnnotation(annoId).nonEmpty))

    def uniqueId : qq.reflect.Symbol = 
      val ids = idFieldsCode
      ids match   
        case List(e) => e
        case o => throw new IllegalStateException(s"uniqueId must be call with one id fieds, here we have this type: ${tpe}\n this id fields : ${o}")

    def uniqueIdString : Expr[String] = 
      Expr(uniqueId.name)
    def idString: Expr[List[String]] = 
      Expr(idFieldsCode.map(_.name))

    def uniqueIdValueCode[E,ID](e : Expr[E]):Expr[Any] =
      val idFieldName = uniqueId.name
      println(idFieldName)
      println(fields.find(_.name == idFieldName))
      fields.find(_.name == idFieldName).map(f => Select(e.asTerm, f).asExpr).get


    def readResultBody(r : Expr[ResultSet],offset : Expr[Int]):Expr[Seq[Any]]=
        '{for (i : Int <- 1 to ${GenMacro.countFields()})
          yield ${r}.getObject(i + ${offset})
        }

    def createFunctionBody[A](param: Expr[A]): Expr[String] = 
      
      val accessors : List[Expr[_]] = fields.flatMap(f => List(Expr(f.name),Select(param.asTerm, f).asExpr))
      val l = Expr.ofList(accessors)
      '{
        ${l}.mkString(",")
       }
    def fillInsertBody[A](param: Expr[A],stmt: Expr[PreparedStatement]): Expr[Unit] = 
      
      val accessors : List[Expr[_]] = fields.map(f => Select(param.asTerm, f).asExpr)
      val l = Expr.ofList(accessors)
      '{
        ${l}.zipWithIndex.map{ (e,i) =>
          ${stmt}.setObject(i+1,e)
        }
        ()  
       }
   
