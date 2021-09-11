package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.model.Model.{Event, Groupe, Message, Room, RoomMessage, User, UserRoom, UserUserMessage}
import bon.jo.datamodeler.model.macros.GenMacro
import bon.jo.datamodeler.model.sql.Dao
import bon.jo.datamodeler.service.Service
import bon.jo.datamodeler.util.{ConnectionPool, Pool}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.sql.Connection
import java.time.LocalDateTime

class TestMessage extends AnyFlatSpec with should.Matchers:

  given Pool[java.sql.Connection] = ConnectionPool(10)("jdbc:sqlite:sample.db","org.sqlite.JDBC")
  given Dao.IntDaoSync[Room] = Dao.IntDaoSync[Room]((id, e ) => e.copy(id = id) )
  given Dao.IntDaoSync[User] = Dao.IntDaoSync[User]((id, e ) => e.copy(id = id) )
  given Dao.IntDaoSync[Message] = Dao.IntDaoSync[Message]((id, e ) => e.copy(id = id) )
  given RawDao.Dao[RoomMessage] = RawDao[RoomMessage]
  given RawDao.Dao[UserUserMessage] = RawDao[UserUserMessage]
  import bon.jo.datamodeler.util.ConnectionPool.*
  "A user" should "can send message" in {



    given  Connection =  pool.get

    SimpleSql.stmt{

      ( SimpleSql.dropTable[Room])
      ( SimpleSql.createTable[Room])
      ( SimpleSql.dropTable[User])
      ( SimpleSql.createTable[User])
      ( SimpleSql.dropTable[Message])
      ( SimpleSql.createTable[Message])
      ( SimpleSql.dropTable[RoomMessage])
      ( SimpleSql.createTable[RoomMessage])
      ( SimpleSql.dropTable[UserUserMessage])
      ( SimpleSql.createTable[UserUserMessage])

      SimpleSql.thisStmt.close

    }
    def test =


      val service = Service()
      import service.*
      import Dao.IntEntityMethods.*

      var u1 = User(name = "u1",email = "test@test")
      var u2 = u1.copy(name = "u2")
      u1 = u1.save()
      u2 = u2.save()

      println(u1)
      println(u2)

      for(_ <- 1 to 4)
        u1.sendToUser(Message(content = "salut u2"),2)
        u2.send(Message(content = "meci u1"),u1)
      println(u2.readMessages())
      println(u1.readMessages())




    end test

    inline def now =  System.currentTimeMillis

    val t  = now
    try
      test
      println(now - t)

    finally
      pool.closeAll()
  }


