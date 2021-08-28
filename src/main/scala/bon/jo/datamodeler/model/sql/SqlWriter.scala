package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.model.sql.SimpleSql.User
import bon.jo.datamodeler.model.sql.SqlWriter.{/, UsingSb,  writer}
import bon.jo.datamodeler.model.macros.SqlMacro
import bon.jo.datamodeler.model.macros.GenMacro
import bon.jo.datamodeler.util.ConnectionPool
import bon.jo.datamodeler.util.Pool
import java.sql.Connection
import java.time.LocalDateTime




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







case class Group(id : Int,name : String)
type -[A] = A ?=> A 
inline def sql[E] :  -[Sql[E]] = summon

object T extends App:

  inline def lToUser(raw : List[Any]):User = User(raw(0).asInstanceOf ,raw(1).asInstanceOf,raw(2).asInstanceOf,raw(3).asInstanceOf)
  given (List[Any] => User) = e => lToUser(e)
  given (List[Any] => Event) = raw =>  Event(raw(0).asInstanceOf ,LocalDateTime.parse(raw(1).toString))
  case class Event(id : Int,time : LocalDateTime = LocalDateTime.now)
  object UserSql extends Sql[User]
  object EventSql extends Sql[Event]
  import UserSql.*
  given Sql[User] = UserSql
  given Sql[Event] = EventSql
  given StringBuilder = StringBuilder()
  given Pool[java.sql.Connection] = ConnectionPool(10)("jdbc:sqlite:sample.db","org.sqlite.JDBC")
  object daoUser extends DaoSync[User,Int]
  object eventDao extends DaoSync[Event,Int]
    //  SimpleSql.

  import ConnectionPool.*

  given  Connection =  pool.get
    def t1 =
      selectMe
      select[Group]
      from
      join[Group](_.groupe,_.id)
      where(z => z.groupe) === 1
      or(_.groupe)  === 2

    def t2 =

      try
    
        val t = SimpleSql.stmt{ 
          
          println( SimpleSql.dropTable[User])
          println( SimpleSql.createTable[User])
          println( SimpleSql.dropTable[Event])
          println( SimpleSql.createTable[Event])
          SimpleSql.thisStmt.close
          
        }
        eventDao.insert(Event(1))
        println( eventDao.select(_.id,1))
        insert
        value
        println(writer)
        println(t)
        SimpleSql.prepStmt(writer.toString){
          val t = System.currentTimeMillis
          val f = SqlMacro.fillInsert[User]
          //val readAllUser = SimpleSql.readAll[User]
      
          for(t <- 1 to 1)
            f(User(t,"test",1,"sdfsdf"),SimpleSql.thisPreStmt)
            SimpleSql.thisPreStmt.addBatch
          SimpleSql.thisPreStmt.executeBatch
          
          println( System.currentTimeMillis - t)
          writer.clear

        
        
        }
        selectMe 
        from
        where(_.id) === 1
        println(writer.toString)
        SimpleSql.prepStmt(writer.toString){
          val res = SimpleSql.thisPreStmt.executeQuery()
          val readUser = SqlMacro.readResultSet[User]
          res.next()
          println(lToUser(readUser(res)))
          SimpleSql.thisPreStmt.close
        }
        
        def test = 
          println(daoUser.select(_.id,1))

        daoUser.insert(User(2,"totototo",1,"sdfsdf"))
        println("id 2 : ")
        println(daoUser.select(_.id,2))
        test
        pool.release
        
        println(daoUser.select(_.id,2))
        println(daoUser.select(_.id,1))
        println(daoUser.max(_.id))
        println(daoUser.selectAll())
      finally 
        pool.closeAll()
    // fill(User("test",1,"sdfsdf"))
    end t2
    t2




  
  





