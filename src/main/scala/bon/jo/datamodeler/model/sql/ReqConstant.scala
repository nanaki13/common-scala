package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.util.Utils
import bon.jo.datamodeler.util.Utils.{writer, /,-}

case class ReqConstant(
    insertString: String,
    whereIdString: String,
    selectAllString: String,
    deleteIdString: String,
    deleteString: String,
    updateString: String
):
  val updateById = updateString + whereIdString
  val selectById = selectAllString + whereIdString
  println(this)

object ReqConstant:

  type Str[E] = Sql[E] ?=> String
  inline def sqlImpl[A]: -[Sql[A]] = summon[Sql[A]]
  inline def insertString[E]: Str[E] = {
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

  inline def apply[E]()(using Sql[E]): ReqConstant =
    ReqConstant(
      insertString,
      whereIdString,
      selectAllString,
      deleteIdString,
      deleteString,
      updateString
    )
