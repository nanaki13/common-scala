package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.model.Model.{Event, Groupe, Room, User, UserRoom}
import bon.jo.datamodeler.model.Page
import bon.jo.datamodeler.model.macros.GenMacro
import bon.jo.datamodeler.model.sql.DaoInline
import bon.jo.datamodeler.model.sql.Filtre.*
import bon.jo.datamodeler.util.ConnectionPool.{pool, *}
import bon.jo.datamodeler.util.Utils.writer
import bon.jo.datamodeler.util.{ConnectionPool, Pool}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*

import java.sql.Connection
import java.time.LocalDateTime
import java.util.Date

class TestMacro extends AnyFlatSpec with should.Matchers:

  case class ManyType( s : String, p : Int,u : LocalDateTime,ss : Date,l : List[String])
  "A macro helper" can " get fields info of a type" in {
      val infoManyType = GenMacro.fieldsClass[ManyType]()
      println(infoManyType)

      infoManyType.list.find(_.name == "p").get.parse("1") should be (1)
      infoManyType.list.find(_.name == "s").get.parse("1") should be ("1")
      infoManyType.list.find(_.name == "u").get.parse("2021-12-12T12:12:12") should be (LocalDateTime.parse("2021-12-12T12:12:12"))
  }


