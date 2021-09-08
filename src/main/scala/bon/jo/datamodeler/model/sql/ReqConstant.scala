package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.model.macros.{GenMacro, SqlMacro}
import bon.jo.datamodeler.util.Alias.nextAlias
import bon.jo.datamodeler.util.{Alias, Utils}
import bon.jo.datamodeler.util.Utils.{-, /, writer}


case class ReqConstant[E](
    insertString: String,
    whereIdString: String,
    selectAllString: String,
    deleteIdString: String,
    deleteString: String,
    updateString: String,
    columns: List[String],
    alias : String,table : String):
  val updateById = updateString + whereIdString
  val selectById = selectAllString + whereIdString
  val columnsComa = columns.mkString(", ")
  val columnsAlias = columns.map(s"$alias."+_).mkString(", ")
  val from = s"FROM $table"
  val tableAlias = s"$table $alias"
  val fromTableAlias = s"FROM $tableAlias"


object ReqConstant:

  type Str[E] = Sql[E] ?=> String
  type ListString[E] = Sql[E] ?=> List[String]
  inline def sqlImpl[A]: -[Sql[A]] = summon[Sql[A]]

  def selectJoin(left : ReqConstant[_], right : ReqConstant[_],leftCol : String,rightCol:String) =
    s"SELECT ${left.columnsAlias},${right.columnsAlias} FROM ${left.tableAlias} JOIN ${right.tableAlias} ON ${left.alias}.$leftCol = ${right.alias}.$rightCol"
  def join(left : ReqConstant[_], right : ReqConstant[_],leftCol : String,rightCol:String) =
    s"JOIN ${right.tableAlias} ON ${left.alias}.$leftCol = ${right.alias}.$rightCol"
  inline def insertString[E] : Str[E] = {
    Utils.stringBuilder {
      sqlImpl.insert
      sqlImpl.value
      writer.toString
    }
  }

  inline def whereIdString[E]: Str[E] = {
    Utils.stringBuilder {
      /(" WHERE (")
      sqlImpl.idClause
      /(")")
      writer.toString
    }
  }
  inline def selectAllString[E]: Str[E] =
    Utils.stringBuilder {
      sqlImpl.selectMe
      sqlImpl.from
      writer.toString
    }

  inline def deleteIdString[E]: Str[E] =
    Utils.stringBuilder {
      sqlImpl.delete
      /(" WHERE ")
      sqlImpl.idClause
    }

  inline def deleteString[E]: Str[E] =
    Utils.stringBuilder {
      sqlImpl.delete
    }

  inline def updateString[E]: Str[E] =
    Utils.stringBuilder {
      sqlImpl.update
    }

  inline def columns[E]: ListString[E] =
      SqlMacro.columnsNameList[E]


  inline def apply[E]()(using Sql[E], Alias): ReqConstant[E] =
    ReqConstant[E](
      insertString,
      whereIdString,
      selectAllString,
      deleteIdString,
      deleteString,
      updateString,
      columns,
      Alias.nextAlias,
      SqlMacro.tableName[E].name
    )
