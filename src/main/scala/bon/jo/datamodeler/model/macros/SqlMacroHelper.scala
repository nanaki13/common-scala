package bon.jo.datamodeler.model.macros

import bon.jo.datamodeler.util.Utils.{/, writer}

import scala.quoted.{Expr, Quotes, ToExpr, Type, quotes}
import java.sql.PreparedStatement
import java.sql.ResultSet
import java.time.{LocalDate, LocalDateTime}
class SqlMacroHelper[Q <: Quotes, T : Type]()(using val qq : Q) :
    import  qq.reflect.*
    
    lazy val tpe: TypeRepr = TypeRepr.of[T]
    lazy val symbol = tpe.typeSymbol
    lazy val tree = TypeTree.of[T]
    lazy val fields : List[qq.reflect.Symbol] = symbol.caseFields
    lazy val constructor = symbol.primaryConstructor
    lazy val  name = symbol.name

    lazy val constructorParamLists: List[List[Symbol]] = constructor.paramSymss


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
      fields.find(_.name == idFieldName).map(f => Select(e.asTerm, f).asExpr).get

    def idsValueCode[E,ID](e : Expr[E]):Expr[List[Any]] =
      val idFieldName = idFieldsCode.map(_.name)
      Expr.ofList(fields.filter(field => idFieldName.contains(field.name)).map(f => Select(e.asTerm, f).asExpr))


    def readResultBody(r : Expr[ResultSet],offset : Expr[Int]):Expr[Seq[Any]]=
        '{for (i : Int <- 1 to ${GenMacro.countFields()})
          yield ${r}.getObject(i + ${offset})
        }

    def readResultToBody(r : Expr[ResultSet],offset : Expr[Int]):Expr[T]=
      GenMacro.listToCode(mapSqlJavaToJava(readResultBody(r ,offset )))

    def mapSqlJavaToJava(seq : Expr[Seq[Any]]) : Expr[Seq[Any]] =
      val mappinFunctio =  Expr.ofSeq(fields.map(tpe.memberType(_).asType).map(
       (t) =>
          t match
          case '[LocalDateTime] =>
            '{
               (i: Int) =>
                 $seq(i) match
                  case a  : LocalDateTime => a
                  case a  : Any  if a != null => LocalDateTime.parse(a.toString)
                  case _ => null
             }
          case _ =>
             '{ (i: Int) => ${seq}(i)}

        ).toSeq)
        '{
            ($mappinFunctio.zipWithIndex).map (_(_))
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

    def fillPreparedStatmentWithUniqueId[T: Type](value : Expr[T],osffset : Expr[Int],stmt : Expr[PreparedStatement])(using  Quotes): Expr[Unit] =
      '{
      $stmt.setObject($osffset,${uniqueIdValueCode(value)})
      ()
      }
    def fillPreparedStatmentWithId[T: Type](value : Expr[T],osffset : Expr[Int],stmt : Expr[PreparedStatement])(using  Quotes): Expr[Unit] =
      '{
        ${idsValueCode(value)}.zipWithIndex.foreach{
          (v ,i ) => $stmt.setObject(i+$osffset,v)
        }
        ()
      }

    def sqlTypesDefCode:Expr[List[String]] =

      import bon.jo.datamodeler.model.sql.SimpleSql.id
      val tpe: TypeRepr = TypeRepr.of[T]
      val sbe: Symbol = TypeRepr.of[id].typeSymbol
      val symbol = tpe.typeSymbol
      val fields = symbol.caseFields
      val ids = idFieldsCode

      val monoId  = ids.size == 1

      // val idFieldsSymbols = symbol.primaryConstructor.paramSymss.flatMap(_.filter(_.getAnnotation(sbe).nonEmpty))
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
          case '[LocalDateTime] =>  /(" DATETIME")
        if monoId && ids.find(_.name == f.name).isDefined
        then
          /(" PRIMARY KEY")

     
        writer.toString
      )
      Expr(f)
   
