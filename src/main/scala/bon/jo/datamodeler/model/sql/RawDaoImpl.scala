package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.model.macros.{GenMacro, SqlMacro}
import bon.jo.datamodeler.util.ConnectionPool.{onPreStmt, onStmt}
import bon.jo.datamodeler.util.Utils.{/, writer}
import bon.jo.datamodeler.util.{Pool, Utils}

import java.sql.ResultSet

trait RawDaoImpl[E](using Pool[java.sql.Connection], Sql[E]) extends RawDao[E]:
  val reqConstant: ReqConstant[E]
  val compiledFunction: CompiledFunction[E]
  extension (r: ResultSet)
    def iterator[A](read: ResultSet => A): Iterator[A] =
      new Iterator[A] {

        inline def hasNext(): Boolean =
          val ret = r.next
          ret
        inline def next(): A = read(r)
      }
  inline def max[T](inline fSel: E => T): W[T] =
    wFactory(onStmt {
      sqlImpl.max(fSel)

      val resQ = SimpleSql.thisStmt.executeQuery(writer.toString)
      resQ.next
      val res = resQ.getObject(1)
      res.asInstanceOf[T]
    })

  inline def select(inline fSel: E => Any, value: Any)(using
      translate: Seq[Any] => E
  ): W[Option[E]] =
    wFactory {
      val selSql = Utils.stringBuilder {
        sqlImpl.selectMe
        sqlImpl.from
        sqlImpl.where(fSel) === "?"
      }

      onPreStmt(selSql) {

        SimpleSql.thisPreStmt.setObject(1, value)
        val res = SimpleSql.thisPreStmt.executeQuery()
        if (res.next()) then

          Some((translate(compiledFunction.readResultSet(res, 0))))
        else None
      }
    }
  inline def fillInsert = compiledFunction.fillInsert

  inline def insert(e: E): W[Int] =
    wFactory(onPreStmt(reqConstant.insertString) {
      fillInsert(e, SimpleSql.thisPreStmt)
      SimpleSql.thisPreStmt.executeUpdate
    })

  inline def insertAll(es: Iterable[E]): W[Int] =
    wFactory {
      onPreStmt(reqConstant.insertString) {
        for (e <- es)
          fillInsert(e, SimpleSql.thisPreStmt)
          SimpleSql.thisPreStmt.addBatch()
        SimpleSql.thisPreStmt.executeBatch.sum
      }
    }

  inline def selectAll()(using translate: Seq[Any] => E): W[List[E]] =
    wFactory {
      onStmt {
        val res = SimpleSql.thisStmt.executeQuery(reqConstant.selectAllString)
        res.iterator(r => translate(compiledFunction.readResultSet(r, 0))).toList

      }
    }

  inline def deleteAll(): W[Int] =
    wFactory {
      onStmt {
        SimpleSql.thisStmt.executeUpdate(reqConstant.deleteString)
      }
    }

  inline def delete(e: E, inline f: E => Any): W[Int] =
    wFactory {
      val s = Utils.stringBuilder {
        sqlImpl.delete
        /(" WHERE ")
        sqlImpl.columnName(f)
        /(" = ? ")
      }
      println(s)
      onPreStmt(s) {
        SimpleSql.thisPreStmt.setObject(1, f(e))
        SimpleSql.thisPreStmt.executeUpdate()
      }
    }

  inline def join[B, IDB](
      inline fk: E => IDB
  )(using Dao.Sync[B, IDB], Seq[Any] => E, Seq[Any] => B): W[List[(E, B)]] =
    wFactory {
      val otherDao: Dao.Sync[B, IDB] = summon[Dao.Sync[B, IDB]]
      val l: Seq[Any] => E = summon[Seq[Any] => E]
      val r: Seq[Any] => B = summon[Seq[Any] => B]
      val join = ReqConstant.selectJoin(
        reqConstant,
        otherDao.reqConstant,
        GenMacro.fieldSelection(fk)._2,
        SqlMacro.uniqueIdString[B]
      )

      def readResulsetJoin(resultSet: ResultSet): (E, B) =
        (
          l(compiledFunction.readResultSet(resultSet, 0)),
          r(otherDao.compiledFunction.readResultSet(resultSet, reqConstant.columns.size))
        )

      onStmt {
        val res = SimpleSql.thisStmt.executeQuery(join)
        res.iterator(readResulsetJoin).toList

      }
    }