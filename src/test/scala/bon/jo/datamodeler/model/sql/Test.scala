package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.model.Model.{Event, Groupe, Room, User, UserRoom}
import bon.jo.datamodeler.model.macros.GenMacro
import bon.jo.datamodeler.model.sql.Dao
import bon.jo.datamodeler.util.Utils.writer
import bon.jo.datamodeler.util.{ConnectionPool, Pool}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.sql.Connection
import java.time.LocalDateTime

class Test extends AnyFlatSpec with should.Matchers:
  "A dao" should "can save, update, delete ..." in {

    given (Seq[Any] => User) = GenMacro.listToFunction[User]
    given (Seq[Any] => Event) = raw =>  Event(raw(0).asInstanceOf ,LocalDateTime.parse(raw(1).toString))
    given (Seq[Any] => Groupe) = GenMacro.listToFunction[Groupe]
    given (Seq[Any] => UserRoom) = GenMacro.listToFunction[UserRoom]
    given (Seq[Any] => Room) = GenMacro.listToFunction[Room]

    given StringBuilder = StringBuilder()
    given Pool[java.sql.Connection] = ConnectionPool(10)("jdbc:sqlite:sample.db","org.sqlite.JDBC")
    given Sql[Event] = Sql()
    given Sql[User] = Sql()
    given Sql[Room] = Sql()
    given Sql[Groupe] = Sql()
    given Sql[UserRoom] = Sql()
    given daoRoom :  Dao.IntDaoSync[Room] = Dao.IntDaoSync[Room]((id, e ) => e.copy(id = id) )
    given daoUser : Dao.IntDaoSync[User] = Dao.IntDaoSync[User]((id, e ) => e.copy(id = id) )
    given eventDao : Dao.IntDaoSync[Event] = Dao.IntDaoSync[Event]((id, e ) => e.copy(id = id) )
    given groupDao : Dao.IntDaoSync[Groupe] = Dao.IntDaoSync[Groupe]((id, e ) => e.copy(id = id) )

    val linkDao : RawDao.Dao[UserRoom] = RawDao[UserRoom]
    //  SimpleSql.
 //   given userRoom : DaoSync.IntDaoSync[UserRoom] = DaoSync.IntDaoSync.apply[UserRoom]((id,e ) => e.copy(id = id) )
    import bon.jo.datamodeler.util.ConnectionPool.*

    given  Connection =  pool.get


    def t2 =


      val t = SimpleSql.stmt{

        ( SimpleSql.dropTable[User])
        ( SimpleSql.createTable[User])
        ( SimpleSql.dropTable[Event])
        ( SimpleSql.createTable[Event])
        ( SimpleSql.dropTable[Room])
        ( SimpleSql.createTable[Room])
        ( SimpleSql.dropTable[UserRoom])
        ( SimpleSql.createTable[UserRoom])
        SimpleSql.thisStmt.close

      }


      println(daoUser.select(_.id,1))
      val( users,evs) =
        (for (i <- 1 to 10)
          yield (User( i,"totototo","sdfsdf"),Event( i))).unzip
      daoUser.insertAll(users) should be (users.size)
      eventDao.insertAll(evs) should be (evs.size)
      println("id 2 : ")
      println(daoUser.select(_.id,2))
      daoUser.saveAll(users) should be (users.size)
      eventDao.saveAll(evs) should be (evs.size)

      val u  = User( 1,"totototo","sdfsdf")
      daoUser.delete(u) should be (1)
      println("id clause : "+daoUser.delete(u,_.name))

      var user  =  User( 1,"Jon","sdfsdf")


      {
        import Dao.EntityMethods.*
        user.save() should be (1)


        user.save() should be (1)
        for(i <- 1 to 10)
          var room  =  Room( i,"r")
          val userRoom = UserRoom(user.id,room.id)
          room.insert() should be (1)
          linkDao.insert(userRoom)
        println(linkDao.join[Room,Int](_.idRoom))
      }




      user = user.copy(name = "Bill")

      daoUser.update(user) should be (1)
      (daoUser.select(_.id,1)).get should be (user)


     // println(daoUser.join[Groupe,Int](_.groupe))
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


