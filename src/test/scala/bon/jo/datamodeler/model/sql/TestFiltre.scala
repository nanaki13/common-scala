package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.model.Model.{Event, Groupe, Room, User, UserRoom}
import bon.jo.datamodeler.model.{Page, sql}
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

class TestFiltre extends AnyFlatSpec with should.Matchers :


  "A filtre" can " be created" in {
      import Filtre.*
      val filtre : BooleanFiltre =  "name".field < "tot".exp
      given Def[User] = Def()
      val typedFiltre = filtre.typed[User]
      typedFiltre.isDefined should be (true)
      filtre.value should be ("(name < 'tot')")
  }


