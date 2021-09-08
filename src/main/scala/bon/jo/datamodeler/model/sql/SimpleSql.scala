package bon.jo.datamodeler.model.sql

import java.sql.{Connection, DriverManager, Statement}
import scala.reflect.ClassTag
import java.sql.PreparedStatement
import bon.jo.datamodeler.model.macros.{GenMacro, SqlMacro}

import scala.annotation.tailrec
import scala.annotation.StaticAnnotation
import bon.jo.datamodeler.model.Model.User
object SimpleSql {

  //Class.forName("org.sqlite.JDBC")
  def connect[A](string: String = "jdbc:sqlite:sample.db")(trt :C[A]) : A =
    given Connection = DriverManager.getConnection(string)
    val res = trt
    thisCon.close
    res
  def stmt[A](trt : S[A]): C[A]=
    given Statement = summon[Connection].createStatement()
    trt
   def prepStmt[A](s : String)(trt : SP[A]): C[A]=

    given PreparedStatement = summon[Connection].prepareStatement(s)
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
  final class id extends StaticAnnotation

  inline def dropTable[T]:S[Unit] = 
    val tableName = SqlMacro.tableName[T]
    val statlment = s"drop table if exists ${tableName.name}"
   
    thisStmt.executeUpdate(statlment)
  
  inline def createTable[T]:S[Unit] = 
    val tableName = SqlMacro.tableName[T]
    val typesDef : List[Any] = SqlMacro.sqlTypesDef[T]

    val ids = SqlMacro.idsString[T]
    val statlment = s"""
|CREATE TABLE ${tableName.name} (
|   ${typesDef.mkString(", ")}
|   ${
      if(ids.length > 1 )
      then s",PRIMARY KEY (${ids.mkString(", ")})"
      else ""

    }
|)
|
    """.stripMargin


    GenMacro.log(statlment)
    thisStmt.executeUpdate(statlment)

  

}
