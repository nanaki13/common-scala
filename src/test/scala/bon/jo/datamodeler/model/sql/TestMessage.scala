package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.model.Model.{Event, Groupe, Message, Room, RoomMessage, User, UserRoom, UserUserMessage}
import bon.jo.datamodeler.model.macros.GenMacro
import bon.jo.datamodeler.model.sql.Dao
import bon.jo.datamodeler.service.Service
import bon.jo.datamodeler.util.{ConnectionPool, Pool}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import scala.concurrent.ExecutionContext.Implicits.global

import java.sql.Connection
import java.time.LocalDateTime
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

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

    ConnectionPool.onStmt{

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

      def lcnh = for(_ <- 1 to 100)
        u1.sendToUser(Message(content = "salut u2"),2)
        u2.send(Message(content = "meci u1"),u1)

      val f1 = Future{
        lcnh
      }
      val f2 = Future{
        lcnh
      }
      val f = Future.sequence(Seq(f1,f2))
      Await.result(f,Duration.Inf)
      u2.readMessages()
      u1.readMessages()




    end test

    inline def now =  System.currentTimeMillis

    val t  = now
    try
      test
      println(now - t)

    finally
      pool.closeAll()
  }


