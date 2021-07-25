package bon.jo.datamodeler.model

import java.sql.{Connection, DriverManager, Statement}
import scala.reflect.ClassTag
import java.sql.PreparedStatement

object SimpleSql {

  Class.forName("org.sqlite.JDBC")
  def connect[A](string: String = "jdbc:sqlite:sample.db")(trt :C[A]) : A =
    given Connection = DriverManager.getConnection(string)
    val res = trt
    thisCon.close
    res
  def stmt[A](trt : S[A]): C[A]=
    given Statement = summon[Connection].createStatement()
    trt
  def thisStmt: S[Statement] = summon
  def thisCon: C[Connection] = summon
  def thisPreStmt: SP[PreparedStatement] = summon
  type C[A] = Connection ?=> A
  type S[A] = java.sql.Statement ?=> A
  type SP[A] = java.sql.PreparedStatement ?=> A


  inline def ps : SP[PreparedStatement] = summon
  inline def mapping[A](using DBMapping[A]) : DBMapping[A] = summon
  trait DBMapping[A]:
    val columns : List[String]
    val table : String
    val reader : (A,Int) => Any
    def prepareQuestionMark = ("?" * columns.size).mkString(", ")
    def listComa = columns.map(c => s"`$c`").mkString(", ")

    def read(a : A):Iterable[Any] = for(i <- 0 until columns.size) yield reader(a,i)
    
  extension (s : String)
    inline def prepare():  C[PreparedStatement]=
      thisCon.prepareStatement(s)
  def prepareInsert[A](using DBMapping[A]):C[PreparedStatement]=
    val mapping_ = mapping
    val str = s"INSERT INTO ${mapping_.table} (${mapping_.listComa}) VALUES (${mapping_.prepareQuestionMark})"
   
    str.prepare()
  
  def insert[A](a : A)(using DBMapping[A]): SP[Unit] = 
    mapping.read(a).zipWithIndex.map((v,i) => fillSp(v,i+1))
    ps.addBatch
  def fillSp(a : Any,col : Int) : SP[Unit] = ps.setObject(col,a)
  def executeBatch():SP[Unit] = thisStmt.executeBatch

  class ProductReader[A <: Product](table_ : String)(using A) extends DBMapping[A]:
    inline def rep(using A) :A = summon
    val columns: List[String] = rep.productElementNames.toList
    val reader: (A, Int) => Any = (a,i)=> a.productElement(i)
    val table: String = table_

  case class User(name : String, groupe : Int)

  
  @main def testB  = 
    import bon.jo.datamodeler.model.Dsl.*
    import bon.jo.datamodeler.model.ToSql.toSqlCreate



    import SimpleSql.*
    val update = connect("jdbc:sqlite:sample.db"){
      //  val updateRes= stmt{
         // thisStmt.executeUpdate(userEntity.toSqlCreate())
         // thisStmt.close
      


       // }
        given User = User("",0)
        given  ProductReader[User] =  ProductReader[User]("user")
        given PreparedStatement = prepareInsert[User]
        insert(User("test",1))
        executeBatch()
      //  println(updateRes)
        thisCon.close
      }
      println(update)
}
