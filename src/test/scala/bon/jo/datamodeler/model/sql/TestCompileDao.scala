package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.model.Model.{Event, Groupe, Room, User, UserRoom}
import bon.jo.datamodeler.model.macros.GenMacro
import bon.jo.datamodeler.model.sql.DaoInline
import bon.jo.datamodeler.util.Utils.{UsingSb, writer}
import bon.jo.datamodeler.util.{ConnectionPool, Pool}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.sql.Connection
import java.time.LocalDateTime

class TestCompileDao extends AnyFlatSpec with should.Matchers:
  "A inline raw dao" can " compile " in {



    given StringBuilder = StringBuilder()


    given Pool[java.sql.Connection] = ConnectionPool(10)("jdbc:sqlite:sample2.db","org.sqlite.JDBC")



    given a : RawDaoOps.Sync[UserRoom] =  RawDao.Sync.fromPool[UserRoom]( )

    def insert[E](ent : E)(using d : RawDaoOps.Sync[E]) = d.insert(ent)
    import bon.jo.datamodeler.util.ConnectionPool.*

    given  Connection =  pool.get
    SimpleSql.stmt{


      ( SimpleSql.dropTable[UserRoom])
      ( SimpleSql.createTable[UserRoom])
      SimpleSql.thisStmt.close

    }
    pool.release

    def test =
      insert(UserRoom(1,1)) should be (1)
    end test


    inline def now =  System.currentTimeMillis

    val t  = now
    try
      test
      println(now - t)

    finally

      pool.closeAll()
  }


