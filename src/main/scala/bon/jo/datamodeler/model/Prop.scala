

package bon.jo.datamodeler.model
import _Type.*
case class Prop(name : String,_type : _Type)
case class Entity(name : String,props : List[Prop],pks : List[Prop] = Nil)

object ToSql:
  extension (e : Entity)
    def toSqlCreate():String =
      s"""
         |CREATE TABLE ${e.name}(
         |${e.props.map(_.toSqlDef()).mkString(",\n")})
         |PRIMARY KEY (${e.pks.map(_.toSqlDef()).mkString(",\n")})
         |)
         |""".stripMargin
  extension (e : Prop)
    def toSqlDef():String =
      s"""${e.name} ${e._type.toSqlDef()}"""
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
object Compile:
  import ToScala.*
  extension (e : Entity)
    def compile() =
      import scala.sys.process.*
      println(s"${scala.sys.env("SCALA_HOME")}\\bin\\scalac".!)

object ToScala:
  extension (e : Entity)
    def toScalaCaseClass():String =
      s"""
         |case class ${e.name(0).toUpper}${e.name.tail}(${e.props.map(_.toScalaDef()).mkString(", ")})
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

  type Building[A] = Builder[A] ?=> Builder[A]
  type SBuild[A,B] = Builder[A] ?=> Builder[B]
  def buildingValue[A] : Building[A] = summon
  def numeric(size : Int) : Building[Prop] =
    val b = summon[Builder[Prop]]
    b.value = b.value.copy(_type = Numeric(10))
    b
  def floatNumeric(size : Int) : Building[Prop] =
    val b = summon[Builder[Prop]]
    b.value = b.value.copy(_type = FloatNumeric(10))
    b
  extension (s : String)
    def entity(build : Building[Entity] ) : Builder[Entity]= {
      given Builder[Entity]= Builder(Entity(s,Nil))
      build
    }

    def prop :Building[Entity] =
      val builder = summon[Builder[Entity]]
      val v =builder.value
      builder.value = v.copy(props = v.props :+ Prop(s,null))
      builder
    def _type(_type : _Type) :SBuild[Entity,Prop] =
      val builder = summon[Builder[Entity]]
      val v = builder.value
      val buidedProp = Builder(Prop(s,null))
      builder.value = v.copy(props = v.props :+ buidedProp.value)
      buidedProp
    def prop(build : Building[Prop]) :Building[Entity] =
      val builder = summon[Builder[Entity]]
      val v =builder.value
      given p:  Builder[Prop]= Builder(Prop(s,null))
      val buidedProp = build
      builder.value = v.copy(props = v.props :+ buidedProp.value)
      builder
    def pk(type_ : _Type) :Building[Entity] =
      val builder = summon[Builder[Entity]]
      val v =builder.value
      val cProp =_type(type_)
      builder.value = v.copy(pks = v.pks :+ cProp.value)
      builder




import bon.jo.datamodeler.model.Dsl.*
import ToScala.*
import ToSql.*
@main def test =
  val userEntity = "user".entity{
    "id" pk _Type.Numeric(10)
    "name" _type Text(55)
    "groupe" prop numeric(10)
  }.value

  println(userEntity.toScalaCaseClass())
  println(userEntity.toSqlCreate())
  import Compile.*
  userEntity.compile()
  import SimpleSql.*
  val update = connect("jdbc:sqlite:sample.db"){
    val updateRes= stmt{
      thisStmt.executeUpdate(userEntity.toSqlCreate())
    }
    println(updateRes)
    thisCon.close
  }
  println(update)

