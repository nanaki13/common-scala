package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.model.Model.User
import bon.jo.datamodeler.model.sql.DaoSync
import bon.jo.datamodeler.util.Utils.writer
import bon.jo.datamodeler.util.{ConnectionPool, Pool}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.sql.Connection
import java.time.LocalDateTime

class Test extends AnyFlatSpec with should.Matchers:
  "A dao" should "can save, update, delete ..." in {
    inline def lToUser(raw : Seq[Any]):User = User(raw(0).asInstanceOf ,raw(1).asInstanceOf,raw(2).asInstanceOf,raw(3).asInstanceOf)
    given (Seq[Any] => User) = e => lToUser(e)
    given (Seq[Any] => Event) = raw =>  Event(raw(0).asInstanceOf ,LocalDateTime.parse(raw(1).toString))
    given (Seq[Any] => Groupe) = raw =>  Groupe(raw(0).asInstanceOf ,raw(1).asInstanceOf)
    case class Event(id : Int,time : LocalDateTime = LocalDateTime.now)
    case class Groupe(id : Int, name : String = "Groupe 1")
    given StringBuilder = StringBuilder()
    given Pool[java.sql.Connection] = ConnectionPool(10)("jdbc:sqlite:sample.db","org.sqlite.JDBC")
    given Sql[Event] = Sql()
    given Sql[User] = Sql()
    given Sql[Groupe] = Sql()
    given daoUser : DaoSync.IntDaoSync[User] = DaoSync.IntDaoSync.apply[User]((id,e ) => e.copy(id = id) )
    given eventDao : DaoSync.IntDaoSync[Event] = DaoSync.IntDaoSync.apply[Event]((id,e ) => e.copy(id = id) )
    given groupDao : DaoSync.IntDaoSync[Groupe] = DaoSync.IntDaoSync.apply[Groupe]((id,e ) => e.copy(id = id) )
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
        println( SimpleSql.dropTable[Groupe])
        println( SimpleSql.createTable[Groupe])
        SimpleSql.thisStmt.close

      }


      println(daoUser.select(_.id,1))
      val( users,evs) =
        (for (i <- 1 to 10)
          yield (User( i,"totototo",1,"sdfsdf"),Event( i))).unzip
      daoUser.insertAll(users) should be (users.size)
      eventDao.insertAll(evs) should be (evs.size)
      println("id 2 : ")
      println(daoUser.select(_.id,2))
      daoUser.saveAll(users) should be (users.size)
      eventDao.saveAll(evs) should be (evs.size)

      val u  = User( 1,"totototo",1,"sdfsdf")
      daoUser.delete(u) should be (1)
      println("id clause : "+daoUser.delete(u,_.name))

      var user  =  User( 1,"Jon",1,"sdfsdf")
      import DaoSync.EntityMethods.*
      user.save() should be (1)


      user.save() should be (1)



      user = user.copy(name = "Bill")
      daoUser.update(user) should be (1)
      (daoUser.select(_.id,1)).get should be (user)
      val group  = Groupe(1)
      group.insert() should be (1)
      println(daoUser.join[Groupe,Int](_.groupe))
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


