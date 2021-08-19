package bon.jo.datamodeler.model

import bon.jo.datamodeler.model.SimpleSql.User
import bon.jo.datamodeler.model.SqlWriter.{/, UsingSb, insert, value, writer}
import bon.jo.datamodeler.model.macros.SqlMacro
import bon.jo.datamodeler.model.macros.GenMacro
import bon.jo.datamodeler.util.ConnectionPool
import java.sql.Connection




object SqlWriter:
  type UsingSb[A] = StringBuilder ?=> A
  inline def writer: UsingSb[StringBuilder] = summon
  inline def /(s : Any): UsingSb[Unit] =  writer.append(s)
  case class SelectCase(value: String)(using StringBuilder):

    /("SELECT ")
    /(value)

    def fromCase(f: FromCase): UsingSb[SelectCase] =
      /(" FROM ")
      /(f.value)
      this



    def whereCase(w: Where): UsingSb[SelectCase] =
      /(" WHERE ")
      /(w.value)
      this

  inline def from[T]: UsingSb[Unit] =
    /(" FROM ")
    /(SqlMacro.tableName[T].name)
  inline def where[T](inline f: T => Any): UsingSb[Unit] =
    /(" WHERE ")
    /(GenMacro.fieldSelection[T](f)._2)

  case class FromCase(value: Any)
  case class Where(value: Any)

  inline def createTable[T]: UsingSb[Unit] =
    /("SELECT ")
    /(SqlMacro.columnsName[T])
  inline def select[T]: UsingSb[Unit] =
    /("SELECT ")
    /(SqlMacro.columnsName[T])

  inline def value[T]: UsingSb[Unit] =
    /(" VALUES (")
    /("?," * (GenMacro.countFields[T] - 1))
    /('?' )
    /(')')
  inline def insert[T]: UsingSb[Unit] =
    /("INSERT INTO ")
    /(SqlMacro.tableName[T].name)
    /('(')
    /(SqlMacro.columnsName[T])
    /(')')

  inline def select[T,C]: UsingSb[Unit] =
    /("SELECT ")
    /(SqlMacro.columnsName[T])
    /(SqlMacro.columnsName[C])










trait Sql[A]:
  inline def selectMe : UsingSb[Sql[A]] =
    SqlWriter.select[A]
    this
  inline def select[B] : UsingSb[Sql[A]] =
    /(',')
    /(SqlMacro.columnsName[B])
    this
  inline def from : UsingSb[Sql[A]] =
    SqlWriter.from[A]
    this
  inline def where(inline f: A => Any) : UsingSb[Sql[A]] =
    SqlWriter.where[A](f)
    this

  inline def or(inline f: A => Any) : UsingSb[Sql[A]] =
    /(" OR ")
    /(GenMacro.fieldSelection[A](f)._2)
    this

  inline def join[B](inline f: A => Any,inline g: B => Any): UsingSb[Sql[A]] =
    /(" JOIN ")
    /(SqlMacro.tableName[B].name)
    /(" ON ")
    /(GenMacro.fieldSelection[A](f)._2)
    /(" = ")
    /(GenMacro.fieldSelection[B](g)._2)
    this


  def ===(b: Any): UsingSb[Sql[A]] =
    /(s" = $b")
    this


case class Group(id : Int,name : String)
trait DaoSync[E,ID]:
  def insert(e: E) : E
  
object T extends App:

  object UserSql extends Sql[User]
  import UserSql.*
  given StringBuilder = StringBuilder()

    def t1 =
      selectMe
      select[Group]
      from
      join[Group](_.groupe,_.id)
      where(z => z.groupe) === 1
      or(_.groupe)  === 2

    def t2 =
      given Pool[java.sql.Connection] = ConnectionPool(10)("jdbc:sqlite:sample.db","org.sqlite.JDBC")

    //  SimpleSql.

      insert[User]
      value[User]
      import ConnectionPool.*
      println(writer)

      given  Connection =  pool.get
    
      
      val t = SimpleSql.stmt{ 
        
        println( SimpleSql.dropTable[User])
        println( SimpleSql.createTable[User])
        SimpleSql.thisStmt.close
        
      }
      println(t)
      SimpleSql.prepStmt(writer.toString){
        val t = System.currentTimeMillis
        val f = SqlMacro.fillInsert[User]
        //val readAllUser = SimpleSql.readAll[User]
    
        for(t <- 1 to 1)
          f(User(t,"test",1,"sdfsdf"),SimpleSql.thisPreStmt)
          SimpleSql.thisPreStmt.addBatch
        SimpleSql.thisPreStmt.executeBatch
        SimpleSql.thisPreStmt.close
        println( System.currentTimeMillis - t)
      }
      
    
      pool.release(SimpleSql.thisCon)
    // fill(User("test",1,"sdfsdf"))
    end t2
  println(GenMacro.printTree(User(0,"abc",1,"email")))
  println(GenMacro.testConstructor[User])
  





