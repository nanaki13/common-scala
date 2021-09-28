package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.model.Page
import bon.jo.datamodeler.model.macros.{GenMacro, SqlMacro}
import bon.jo.datamodeler.util.ConnectionPool.{onPreStmt, onStmt}
import bon.jo.datamodeler.util.Utils.{/, writer}
import bon.jo.datamodeler.util.{Pool, Utils}

import java.sql.{PreparedStatement, ResultSet}


object RawDaoInline:


  type Sync[E]  = RawDaoInline[E,CompiledFunction[E]] with RawDaoOpsInline.Sync[E]
  inline def apply[E]()(using Pool[java.sql.Connection]): Sync[E] =
    given Sql[E] = Sql()
    new RawDaoInline[E,CompiledFunction[E]] with RawDaoOpsInline.Sync[E]{
    override val reqConstant: ReqConstant[E] = ReqConstant()
    override val compiledFunction: CompiledFunction[E] = CompiledFunction()

  }
trait RawDaoInline[E,CF <:CompiledFunction[E]](using Pool[java.sql.Connection], Sql[E]) extends RawDaoOpsInline[E]:
  val reqConstant: ReqConstant[E]
  val compiledFunction: CF
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

  inline def select(inline fSel: E => Any, value: Any): W[Option[E]] =
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

          Some((compiledFunction.readResultSet(res, 0)))
        else None
      }
    }
  inline def fillInsert = compiledFunction.fillInsert

  inline def insert(e: E): W[Int] =
    wFactory(onPreStmt(reqConstant.insertString) {
      fillInsert(e, SimpleSql.thisPreStmt)
      val pre : PreparedStatement = SimpleSql.thisPreStmt
      
      SimpleSql.thisPreStmt.executeUpdate
    })

  inline def insertAll(es: Iterable[E]): W[Int] =
    wFactory {
      GenMacro.log(reqConstant.insertString)
      onPreStmt(reqConstant.insertString) {
        for (e <- es)
          fillInsert(e, SimpleSql.thisPreStmt)
          SimpleSql.thisPreStmt.addBatch()
        SimpleSql.thisPreStmt.executeBatch.sum
      }
    }

  inline def selectAll(): W[List[E]] =
    wFactory {
      onStmt {
        val res = SimpleSql.thisStmt.executeQuery(reqConstant.selectAllString)
        res.iterator(r => compiledFunction.readResultSet(r, 0)).toList

      }
    }

  inline def selectAll(page : Page): W[Page.Response[E]] =
    wFactory {
      onStmt {
        val res = SimpleSql.thisStmt.executeQuery(reqConstant.selectCount)
        res.next()
        val count =res.getLong(1)
        val nbCount = count / page.size + 1
        val offset = page.size * page.pageNumber
        val queryString = Utils.stringBuilder {
            /(reqConstant.selectAllString + " ")
              SqlWriter.limit(offset, page.size)
        }

        val resLimited = SimpleSql.thisStmt.executeQuery(queryString)
        Page.Response(res.iterator(r => compiledFunction.readResultSet(r, 0)).toList,page.pageNumber,nbCount.toInt)

      }
    }

  inline def selectAll(page : Page,filtre: Filtre.BooleanFiltre): W[Page.Response[E]] =
    wFactory {
      onStmt {
        val queryCountFilter = Utils.stringBuilder {
          /(reqConstant.selectCount)
          /(" WHERE ")
          /(filtre.value)
        }
        val res = SimpleSql.thisStmt.executeQuery(queryCountFilter)
        res.next()
        val count =res.getLong(1)
        val nbCount = count / page.size + 1
        val offset = page.size * page.pageNumber
        val queryString = Utils.stringBuilder {
          /(reqConstant.selectAllString)
          /(" WHERE ")
          /(filtre.value)
          SqlWriter.limit(offset, page.size)
        }
        GenMacro.log(queryString)

        val resLimited = SimpleSql.thisStmt.executeQuery(queryString)
        Page.Response(res.iterator(r => compiledFunction.readResultSet(r, 0)).toList,page.pageNumber,nbCount.toInt)

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

      onPreStmt(s) {
        SimpleSql.thisPreStmt.setObject(1, f(e))
        SimpleSql.thisPreStmt.executeUpdate()
      }
    }

  inline def join[B, IDB](
      inline fk: E => IDB
  )(using DaoOpsInline.Sync[B, IDB]): W[List[(E, B)]] =
    wFactory {
      val otherDao: DaoOpsInline.Sync[B, IDB] = summon[DaoOpsInline.Sync[B, IDB]]

      val join = ReqConstant.selectJoin(
        reqConstant,
        otherDao.reqConstant,
        GenMacro.fieldSelection(fk)._2,
        SqlMacro.uniqueIdString[B]
      )

      def readResulsetJoin(resultSet: ResultSet): (E, B) =
        (
          compiledFunction.readResultSet(resultSet, 0),
          otherDao.compiledFunction.readResultSet(resultSet, reqConstant.columns.size)
        )

      onStmt {
        println(s"JOIN = $join")
        val res = SimpleSql.thisStmt.executeQuery(join)
        res.iterator(readResulsetJoin).toList

      }
    }

  inline def joinWhere[B, IDB](
                           inline fk: E => IDB,id : IDB
                         )(using DaoOpsInline.Sync[B, IDB]): W[List[(E, B)]] =
    wFactory {
      val otherDao: DaoOpsInline.Sync[B, IDB] = summon[DaoOpsInline.Sync[B, IDB]]
      val fs = GenMacro.fieldSelection(fk)._2
      val join = ReqConstant.selectJoin(
        reqConstant,
        otherDao.reqConstant,
        GenMacro.fieldSelection(fk)._2,
        SqlMacro.uniqueIdString[B]
      )+s" WHERE ${reqConstant.alias}.${GenMacro.fieldSelection(fk)._2} = ?"

      def readResulsetJoin(resultSet: ResultSet): (E, B) =
        (
          compiledFunction.readResultSet(resultSet, 0),
          otherDao.compiledFunction.readResultSet(resultSet, reqConstant.columns.size)
        )
      println(s"JOIN = $join")
      onPreStmt(join) {
        SimpleSql.thisPreStmt.setObject(1,id)
        val res = SimpleSql.thisPreStmt.executeQuery()
        res.iterator(readResulsetJoin).toList

      }
    }

  inline def joinWhereCustom[B, IDB](
                                inline fk: E => IDB,inline field: E => Any,fieldValue : Any
                              )(using DaoOpsInline.Sync[B, IDB]): W[List[(E, B)]] =
    wFactory {
      val otherDao: DaoOpsInline.Sync[B, IDB] = summon[DaoOpsInline.Sync[B, IDB]]
      val fs = GenMacro.fieldSelection(fk)._2
      val join = ReqConstant.selectJoin(
        reqConstant,
        otherDao.reqConstant,
        GenMacro.fieldSelection(fk)._2,
        SqlMacro.uniqueIdString[B]
      )+s" WHERE ${reqConstant.alias}.${GenMacro.fieldSelection(field)._2} = ?"

      def readResulsetJoin(resultSet: ResultSet): (E, B) =
        (
          compiledFunction.readResultSet(resultSet, 0),
          otherDao.compiledFunction.readResultSet(resultSet, reqConstant.columns.size)
        )
      println(s"JOIN = $join")
      onPreStmt(join) {
        SimpleSql.thisPreStmt.setObject(1,fieldValue)
        val res = SimpleSql.thisPreStmt.executeQuery()
        res.iterator(readResulsetJoin).toList

      }
    }
