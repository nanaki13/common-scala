

package bon.jo.datamodeler.model
import _Type.*
import scala.language.dynamics
import java.sql.PreparedStatement
import java.sql.Connection

import bon.jo.datamodeler.model.Dsl.*
import ToScala.*
import ToSql.*


case class Prop(name : String,_type : _Type ,notNull : Boolean = false)
case class Entity(name : String,props : List[Prop],pks : List[Prop] = Nil,links : List[Link]= Nil,nameSpace : String = "")
case class Link(props : List[Prop],fEntity : Entity, linkInEntity : List[Prop])
class Selector(e : Entity)extends scala.Dynamic:
  def selectDynamic(s : String):Prop = e.props.find(_.name == s).get   
extension (e : Entity)
  def _prop : Selector = 
    Selector(e)

@main def test =


  val group = "groupe".entity{
    "id" pk numeric(10)
    "name" _type Text(55)
  }.value

  val userEntity = "user".entity{
    "id" pk numeric(10)
    "name" _type Text(55)
    "groupe" prop(numeric(10),link(group,group._prop.id))
  }.value

  
  /*println(group)
  println(group.toScalaCaseClass())
  println(group.toSqlCreate())
  println(userEntity)
  println(userEntity.toSqlCreate())*/
  import PrepareSql.*

  println(group.createMapper)
  
  import SimpleSql.*
  val update = connect("jdbc:sqlite:sample.db"){
    val updateRes= stmt{
      thisStmt.executeUpdate(group.toSqlCreate())
      thisStmt.executeUpdate(userEntity.toSqlCreate())
    }
    println(updateRes)
    thisCon.close
  }
  println(update)
import SimpleSql.C
object PrepareSql:
  extension (e : Entity)
    def insert:C[PreparedStatement]=
      val c : Connection= SimpleSql.thisCon
      c.prepareStatement(e.toSqlInsert())
    def createMapper:String = 
      e.props.zipWithIndex.map((e,i) =>s"ps.setObject(${i+1},${e.name})").mkString(";\n")
object ToSql:

  extension (p : Prop)
     def =? = s"""${p.name} = ?"""
  extension (e : Entity)
    def toSqlInsert() : String =  s"""
                          |INSERT INTO ${e.name} (
                          |${e.props.map(_.name).mkString(",\n")}
                          |) VALUES (${("?," * (e.props.size - 1))+"?"})
                          |""".stripMargin
    def toSqlSelect(): String =  s"""SELECT ${e.props.map(_.name).mkString(",\n")}
                            |FROM ${e.name}""".stripMargin
    def where(prop: Prop *) : String = s""" ${toSqlSelect()} WHERE ${prop.map(_.=?).mkString(", ")}"""

    def toSqlCreate():String =
      val pkString =  if e.pks.nonEmpty then s",\nPRIMARY KEY (${e.pks.map(_.name).mkString(",\n")})" else ""
      val fkString =  if e.links.nonEmpty then {
        e.links.map{case Link(props,ent,propss) => s",\nFOREIGN KEY(${props.map(_.name).mkString(",")}) REFERENCES ${ent.name}(${propss.map(_.name).mkString(",")})"}.mkString(",\n")
      } else ""
      s"""
         |CREATE TABLE ${e.name}(
         |${e.props.map(_.toSqlDef()).mkString(",\n")}${pkString}${fkString}
         |)
         |""".stripMargin
    def toSqlDrop():String = s"DROP TABLE IF EXISTS ${e.name}"
  extension (e : Prop)
    def toSqlDef():String =
      s"""${e.name} ${e._type.toSqlDef()} ${if e.notNull then "NOT NULL" else ""}"""



  extension (e : _Type)
    def toSqlDef():String =
      e match
        case _Type.Text(size) => s"VARCHAR($size)"
        case  _Type.Char(size ) =>  s"CHAR($size)"
        case  _Type.Numeric(size) => s"INT($size)"
        case  _Type.FloatNumeric(size) =>  s"FLOAT($size)"
        case  _Type.Boolean => "BYTE(1)"
        case  _Type.Date => "DATE"
        case  _Type.Time => "TIME"
        case  _Type.DateTime => "DATETIME"

object Prop:
  extension (e : Prop)
    def toJavaClass() : String = e._type.toScalaDef()
object Compile:
  import ToScala.*
  extension (e : Entity)
    def compile() =
      import scala.sys.process.*
      println(s"${scala.sys.env("SCALA_HOME")}\\bin\\scalac".!)

object ToScala:
  extension (e : Entity)
    inline def caseClassName :String  = s"${e.name(0).toUpper}${e.name.tail}"
    def toScalaCaseClass():String =
      s"""
         |case class ${caseClassName}(${e.props.map(_.toScalaDef()).mkString(", ")})
         |
         |""".stripMargin
  extension (e : Prop)
    def toScalaDef():String =
      s"""${e.name}: ${e._type.toScalaDef()}"""
  extension (e : _Type)
    def toScalaDef():String =
      e match
        case _Type.Text(size) => "String"
        case  _Type.Char(size ) => "Char"
        case  _Type.Numeric(size) => if size > 4 then "Long" else "Int"
        case  _Type.FloatNumeric(size) => if size > 4 then "Double" else "Float"
        case  _Type.Boolean => "Boolean"
        case  _Type.Date => "java.util.LocalDate"
        case  _Type.Time => "java.util.LocalTime"
        case  _Type.DateTime => "java.util.LocalDateTime"
object Dsl:
  class Builder[A](var value : A)

 
  def thisEntity  :  Building[Entity,Entity] = summon[Builder[Entity]].value
  def thisProp  :  Building[Prop,Prop] = summon[Builder[Prop]].value
  def thisBuilder[A] : OnBuild[A] = summon[Builder[A]]
  type Building[A,B] = Builder[A] ?=> B
  type SubBuild[A,B] =  Building[A,Builder[B]] 
  type OnBuild[A] = Building[A,Builder[A]] 
  type PropB = (Builder[Prop],Builder[Entity])?=> Unit
 // type SBuild[A,B] = Builder[A] ?=> Builder[B]
  
  def buildingValue[A] : Building[A,A] = summon.value
  def numeric(size : Int) : OnBuild[Prop] =
    val b = summon[Builder[Prop]]
    b.value = b.value.copy(_type = Numeric(10))
    b
  def namespace(n : String) : OnBuild[Entity] = 
    val b = thisBuilder
    b.value = b.value.copy(nameSpace = n)
    b
  def link(en : Entity,prop : Prop):PropB = 
    thisBuilder[Entity].value = thisEntity.copy(links = thisEntity.links :+ Link(List(thisProp),en,List(prop)))

    thisBuilder[Entity]

  def floatNumeric(size : Int) : OnBuild[Prop] =
    val b = summon[Builder[Prop]]
    b.value = b.value.copy(_type = FloatNumeric(10))
    b
  extension (s : String)
    def entity(build : OnBuild[Entity] ) : Builder[Entity]= {
      given Builder[Entity]= Builder(Entity(s,Nil))
      build
    }

 
    def _type(_type : _Type) :OnBuild[Entity]=
      val builder = summon[Builder[Entity]]
      
      given Builder[Prop]= Builder(Prop(s,_type))
     // build
      val v = builder.value
      builder.value = v.copy(props = v.props :+ thisProp)
      builder
    def prop( build :PropB) :OnBuild[Entity] =
      val builder = summon[Builder[Entity]]
      given p:  Builder[Prop]= Builder(Prop(s,null))
      build
      builder.value = builder.value.copy(props = builder.value.props :+ p.value)

      builder

    def pk(build : OnBuild[Prop]) :OnBuild[Entity] =
         given p:  Builder[Prop]= Builder(Prop(s,null))
         val buidedProp = build
         buidedProp.value = buidedProp.value.copy(notNull = true)
         val builderE =thisBuilder[Entity]
         builderE.value = builderE.value.copy(props = builderE.value.props :+ buidedProp.value,pks = builderE.value.pks :+ buidedProp.value)
         builderE





