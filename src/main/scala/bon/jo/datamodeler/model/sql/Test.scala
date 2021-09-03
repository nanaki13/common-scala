package bon.jo.datamodeler.model.sql


import bon.jo.datamodeler.model.Model.User
import java.time.LocalDateTime
import bon.jo.datamodeler.util.{ConnectionPool,Pool}
import bon.jo.datamodeler.util.Utils.writer
import bon.jo.datamodeler.model.sql.DaoSync
import java.sql.Connection


object Test {
  
@main def testMacro()=
  inline def lToUser(raw : List[Any]):User = User(raw(0).asInstanceOf ,raw(1).asInstanceOf,raw(2).asInstanceOf,raw(3).asInstanceOf)
  given (List[Any] => User) = e => lToUser(e)
  given (List[Any] => Event) = raw =>  Event(raw(0).asInstanceOf ,LocalDateTime.parse(raw(1).toString))
  case class Event(id : Int,time : LocalDateTime = LocalDateTime.now)

  given StringBuilder = StringBuilder()
  given Pool[java.sql.Connection] = ConnectionPool(10)("jdbc:sqlite:sample.db","org.sqlite.JDBC")
  given Sql[Event] = Sql()
  given Sql[User] = Sql()
  val daoUser = DaoSync.IntDaoSync.apply[User]((id,e ) => e.copy(id = id) )
  val eventDao = DaoSync.IntDaoSync.apply[Event]((id,e ) => e.copy(id = id) )
    //  SimpleSql.

  import bon.jo.datamodeler.util.ConnectionPool.*

  given  Connection =  pool.get

  def testSelect =  
    println(daoUser.select(_.id,2))
    println(daoUser.select(_.id,1))
    println(daoUser.max(_.id))
    println(daoUser.maxId)
   
    println(eventDao.maxId)
  
  def t2 =
    
  
      val t = SimpleSql.stmt{ 
        
        println( SimpleSql.dropTable[User])
        println( SimpleSql.createTable[User])
        println( SimpleSql.dropTable[Event])
        println( SimpleSql.createTable[Event])
        SimpleSql.thisStmt.close
        
      }
      

      println(daoUser.select(_.id,1))
      val( users,evs) = 
        (for (i <- 1 to 10)
        yield (User( i,"totototo",1,"sdfsdf"),Event( i))).unzip
      daoUser.insertAll(users)
      eventDao.insertAll(evs)
      println("id 2 : ")
      println(daoUser.select(_.id,2))
      daoUser.saveAll(users)
      eventDao.saveAll(evs)

      val u  = User( 1,"totototo",1,"sdfsdf")
      println("id clause : "+daoUser.delete(1))
      println("id clause : "+daoUser.delete(u,_.name))
      
      var user  =  User( 1,"Jon",1,"sdfsdf")
      import daoUser.EntityMethods.*
      user.insert()
      user = user.copy(name = "Bill")
      daoUser.update(user)
      println(daoUser.select(_.id,1))
    //  println(eventDao.delete(daoUser.select(_.id,1)))
      
    

    
  // fill(User("test",1,"sdfsdf"))
  end t2
  
  inline def now =  System.currentTimeMillis

  val t  = now
  try
    t2
    println(now - t)
    println(daoUser.selectAll().size)
  finally 
      pool.closeAll()

  

  

}
