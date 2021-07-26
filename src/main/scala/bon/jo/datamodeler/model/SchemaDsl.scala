package bon.jo.datamodeler.model
import bon.jo.datamodeler.model

import java.sql.{Connection, PreparedStatement, ResultSet}
import bon.jo.datamodeler.model.Dsl.*
import bon.jo.datamodeler.model._Type.*
import bon.jo.datamodeler.model.ToSql.*
import bon.jo.datamodeler.model.ToScala.*
import bon.jo.datamodeler.model.SchemaDsl.*
import bon.jo.datamodeler.model.Prop.*
object SchemaDsl:
  inline def entites() : List[Entity] =
    val ent = "entityRaw".entity{
    "id" pk numeric(10)
    "name" _type Text(55)
    "namespace"  _type Text(55)
      }.value

    val prop = "propRaw".entity{
      "id" pk numeric(10)
      "name" _type Text(55)
      "type_size" prop numeric(55)
      "_type" _type Text(55)
      "entity_fk" prop(numeric(10), link(ent,ent._prop.id))
    }.value
    List(ent,prop)

  inline def dll = entites().map(_.toSqlCreate())
  inline def showDll = dll.foreach(println)
  inline def scalaCaseClass = entites().map(_.toScalaCaseClass())
  inline def showScala = scalaCaseClass.foreach(println)
  inline def insertSql = entites().map(_.toSqlInsert())
  inline def showInsert = insertSql.foreach(println)


  def daoTemplate(e : Entity) = 
    

    def setPs : String = e.props.zipWithIndex.map((pr,i) => s"ps.setObject(${i+1},e.${pr.name})").mkString("\n    ")
    def readRs : String = e.props.zipWithIndex.map((pr,i) => s"rs.getObject(${i+1},classOf[${pr.toJavaClass()}])").mkString(",\n    ")


    s"""trait ${e.caseClassName}Dao extends SqlDao[${e.caseClassName}]:
    |   def prepareInsert(using c : Connection) : PreparedStatement = 
    |     c.prepareStatement(\"\""${e.toSqlInsert()}\"\"")
    |   def prepareSelectId(using c : Connection) : PreparedStatement =
    |     c.prepareStatement(\"\""${e.where(e.pks : _ *)}\"\"")
    |   def read(rs : ResultSet): ${e.caseClassName} =
    |     ${e.caseClassName}($readRs)
    |extension (e : ${e.caseClassName})
    |  def save(using PreparedStatement):Int =
    |    val ps = summon
    |    $setPs
    |    ps.executeUpdate()
    |  def read(using PreparedStatement,SqlDao[${e.caseClassName}]): ${e.caseClassName} =
    |    val ps = SimpleSql.thisPreStmt
    |    SqlDao().read(ps.executeQuery())
    |
    |""".stripMargin

object Main:
  var idE = 0
  var idP = 0
  def nIde = 
    idE+=1
    idE
  def nIdp = 
    idP+=1
    idP
  @main def testDll() = 
    //showScala
    SchemaDsl.entites().map(SchemaDsl.daoTemplate).foreach(println)
    def testCreateInsert() =
      SimpleSql.connect[Unit](){

        object daoE extends EntityRawDao
        object daop extends PropRawDao

        entites().foreach(e =>
          println(e.toSqlSelect())
          SimpleSql.stmt{
            SimpleSql.thisStmt.executeUpdate(e.toSqlDrop())
            SimpleSql.thisStmt.executeUpdate(e.toSqlCreate())
          }
          )

        val insertE = daoE.prepareInsert
        val insertP = daop.prepareInsert
        val selectidE = daoE.prepareSelectId
        entites().map(raw).map{
          (e,prs) =>
            given PreparedStatement = insertE
            e.save
            (e,prs)
          }.foreach{ (e,prs) =>
            given PreparedStatement = insertP
            prs.foreach(_.save)
          }
        given PreparedStatement = selectidE
        entites().foreach(e =>
          given EntityRawDao = daoE

          println(raw(e)._1.read)
        )
      }

    end testCreateInsert
   // testCreateInsert()

  end testDll

    

  def raw(e : Entity) : (EntityRaw,List[PropRaw])=
    (EntityRaw(nIde,e.name,e.nameSpace),e.props.map(p => PropRaw(nIdp,p.name,p._type.size,p._type.name.toString,idE)))


case class EntityRaw(id: Long, name: String, namespace: String)

case class PropRaw(id: Long, name: String, type_size: Long, _type: String, entity_fk: Long)

object SqlDao:
  def apply[T]()(using SqlDao[T]) = summon
trait SqlDao[E]:
  def prepareInsert(using c : Connection) : PreparedStatement
  def prepareSelectId(using c : Connection) : PreparedStatement
  def read(rs : ResultSet): E
trait EntityRawDao extends SqlDao[EntityRaw]:
  def prepareInsert(using c : Connection) : PreparedStatement =
    c.prepareStatement("""
INSERT INTO entityRaw (
id,
name,
namespace
) VALUES (?,?,?)
""")
  def prepareSelectId(using c : Connection) : PreparedStatement =
    c.prepareStatement("""SELECT id,
name,
namespace
FROM entityRaw  SELECT id,
name,
namespace
FROM entityRaw WHERE id = ?""")
  def read(rs : ResultSet): EntityRaw =
    EntityRaw(rs.getObject(1,classOf[Long]),
      rs.getObject(2,classOf[String]),
      rs.getObject(3,classOf[String]))
extension (e : EntityRaw)
  def save(using PreparedStatement):Int =
    val ps = summon
    ps.setObject(1,e.id)
    ps.setObject(2,e.name)
    ps.setObject(3,e.namespace)
    ps.executeUpdate()
  def read(using PreparedStatement,SqlDao[EntityRaw]): EntityRaw =
    val ps = SimpleSql.thisPreStmt
    ps.setObject(1,e.id)
    SqlDao().read(ps.executeQuery())


trait PropRawDao extends SqlDao[PropRaw]:
  def prepareInsert(using c : Connection) : PreparedStatement =
    c.prepareStatement("""
INSERT INTO propRaw (
id,
name,
type_size,
_type,
entity_fk
) VALUES (?,?,?,?,?)
""")
  def prepareSelectId(using c : Connection) : PreparedStatement =
    c.prepareStatement("""SELECT id,
name,
type_size,
_type,
entity_fk
FROM propRaw  SELECT id,
name,
type_size,
_type,
entity_fk
FROM propRaw WHERE id = ?""")
  def read(rs : ResultSet): PropRaw =
    PropRaw(rs.getObject(1,classOf[Long]),
      rs.getObject(2,classOf[String]),
      rs.getObject(3,classOf[Long]),
      rs.getObject(4,classOf[String]),
      rs.getObject(5,classOf[Long]))
extension (e : PropRaw)
  def save(using PreparedStatement):Int =
    val ps = summon
    ps.setObject(1,e.id)
    ps.setObject(2,e.name)
    ps.setObject(3,e.type_size)
    ps.setObject(4,e._type)
    ps.setObject(5,e.entity_fk)
    ps.executeUpdate()
  def read(using PreparedStatement,SqlDao[PropRaw]): PropRaw =
    val ps : PreparedStatement = SimpleSql.thisPreStmt
    ps.setObject(1,e.id)
    SqlDao().read(ps.executeQuery())




