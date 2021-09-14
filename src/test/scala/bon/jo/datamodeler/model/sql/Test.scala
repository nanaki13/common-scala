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
  "A dao" can " save, update, delete ..." in {



    given StringBuilder = StringBuilder()
    given Pool[java.sql.Connection] = ConnectionPool(10)("jdbc:sqlite:sample2.db","org.sqlite.JDBC")

    given daoRoom :  Dao.IntDaoSync[Room] = Dao.IntDaoSync[Room]((id, e ) => e.copy(id = id) )
    given daoUser : Dao.IntDaoSync[User] = Dao.IntDaoSync[User]((id, e ) => e.copy(id = id) )
    given eventDao : Dao.IntDaoSync[Event] = Dao.IntDaoSync[Event]((id, e ) => e.copy(id = id) )
    given groupDao : Dao.IntDaoSync[Groupe] = Dao.IntDaoSync[Groupe]((id, e ) => e.copy(id = id) )

    val linkDao : RawDao.Dao[UserRoom] = RawDao[UserRoom]

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
      pool.release


      println(daoUser.select(_.id,1))
      val( users,evs) =
        (for (i <- 1 to 10)
          yield (User( i,"totototo","sdfsdf"),Event( i))).unzip
      daoUser.insertAll(users) should be (users.size)
      eventDao.insertAll(evs) should be (evs.size)
      println("id 2 : ")
      println(daoUser.select(_.id,2))
      daoUser.saveAll(users)
      eventDao.saveAll(evs)

      val u  = User( 1,"totototo","sdfsdf")
      daoUser.delete(u) should be (1)
      println("id clause : "+daoUser.delete(u,_.name))
      val e = eventDao.select(_.id,1)
      var user  =  User( 1,"Jon","sdfsdf")


      {
        import Dao.IntEntityMethods.*
        import bon.jo.datamodeler.model.Model.toRoom
        user = user.save()

        var room  =  Room( 1,"r1")
        var room2  =  Room( 2,"r2")
        room.save()
        room2.save()
        daoUser.deleteAll()
        val users  = for(i <- 1 to 10) yield {
          val userLoop = User(i,s"${i}")
          userLoop.insert() should be (1)
          linkDao.insert(userLoop.toRoom(room))
          userLoop
        }



        for(u <- users)
          val userLoop  = u.copy(id = u.id+15)
          userLoop.insert() should be (1)
          linkDao.insert(userLoop.toRoom(room2)) should be (1)

        println("linDao et room : "+linkDao.join[Room,Int](_.idRoom))
        println("linDao et room : "+linkDao.joinWhere[Room,Int](_.idRoom,1))
        val l : List[(UserRoom,Room)] = linkDao.joinWhere[Room,Int](_.idRoom,1)
        l.map
      }



      user = daoUser.save(user)
      user = user.copy(name = "Bill")

      daoUser.update(user) should be (1)
      (daoUser.select(_.id,user.id)).get should be (user)

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


