package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.util.ConnectionPool.*
import bon.jo.datamodeler.util.Pool
import bon.jo.datamodeler.model.sql.SimpleSql
import bon.jo.datamodeler.model.sql.Sql
import bon.jo.datamodeler.model.macros.{GenMacro, SqlMacro}
import bon.jo.datamodeler.util.Utils.{-, /, UsingSb, writer}
import bon.jo.datamodeler.util.Alias.given
import java.sql.{Connection, PreparedStatement, ResultSet}
import scala.util.Try
import bon.jo.datamodeler.util.Utils






trait Dao[E,ID](using Pool[java.sql.Connection], Sql[E] ) extends DaoOps[E,ID] with  RawDao[E,IdCompiledFunction[E]]:


  extension (e : E)
    inline def __id : Any = compiledFunction.getIdFunction(e)


  override inline def selectById(id: ID): W[Option[E]] =
    wFactory{
      onPreStmt(reqConstant.selectById){
        val resultSet : ResultSet = SimpleSql.doQuery()
        if resultSet.next() then
          Some(compiledFunction.readResultSet(resultSet,1))
        else
          None
      }
    }
  /* inline def max[T](inline fSel : E => T):T =
    onStmt{
      sqlImpl.max(fSel)

      val resQ = SimpleSql.thisStmt.executeQuery(writer.toString)
      resQ.next
      val res = resQ.getObject(1)
      res.asInstanceOf[T]
    }*/
  inline def update(e: E) : W[Int] =
    wFactory {
      println(e)
      println(reqConstant.updateById)
      onPreStmt(reqConstant.updateById) {
        val nbCol = GenMacro.countFields[E]
        compiledFunction.fillUpdate(e, SimpleSql.thisPreStmt)
        compiledFunction.fillPreparedStatmentWithId(e,nbCol + 1,SimpleSql.thisPreStmt)
        SimpleSql.thisPreStmt.executeUpdate
      }
    }


  inline def delete(e: E) : W[Int] =
    wFactory {
      onPreStmt(reqConstant.deleteIdString) {
        compiledFunction.fillPreparedStatmentWithId(e,1,SimpleSql.thisPreStmt)
        SimpleSql.thisPreStmt.executeUpdate
      }
    }


  inline def maxId : W[ID] =
    wFactory {
      onStmt {
        sqlImpl.maxId

        val resQ = SimpleSql.thisStmt.executeQuery(writer.toString)
        resQ.next
        val res = resQ.getObject(1)
        res.asInstanceOf[ID]
      }
    }



  inline def updateAll(es:  Iterable[E]) : W[Int] =
    wFactory {
      onPreStmt(reqConstant.updateById) {
        val nbCol = GenMacro.countFields[E]
        for (e <- es)
          compiledFunction.fillUpdate(e, SimpleSql.thisPreStmt)
          compiledFunction.fillPreparedStatmentWithId(e,1,SimpleSql.thisPreStmt)
          SimpleSql.thisPreStmt.addBatch()
        SimpleSql.thisPreStmt.executeBatch.sum
      }
    }


  inline def update(id : ID,e: E) : W[Int] =
    wFactory {
      onPreStmt(reqConstant.updateById){
        val nbCol = GenMacro.countFields[E]
        fillInsert(e,SimpleSql.thisPreStmt)

        SimpleSql.thisPreStmt.setObject(nbCol+1,id)
        SimpleSql.thisPreStmt.executeUpdate
      }
    }



  inline def deleteById( e : ID) : W[Int] =
    wFactory{
      onPreStmt(reqConstant.deleteIdString){

        SimpleSql.thisPreStmt.setObject(1,e)
        SimpleSql.thisPreStmt.executeUpdate()
      }
    }




end Dao

object Dao:
  inline def dao[E](using RawDao.Dao[E]) : RawDao.Dao[E]= summon
  object EntityMethods:
    extension[E](e : E)(using RawDao.Dao[E])
      inline def insertRaw()  = dao.insert(e)

  object IntEntityMethods:
    extension[E](e : E)(using IntDaoSync[E])
      inline def insert()  = IntDaoSync.dao.insert(e)
      inline def update()  = IntDaoSync.dao.update(e)
      inline def delete()  = IntDaoSync.dao.delete(e)
      inline def save()  = IntDaoSync.dao.save(e)




  object IntDaoSync :
    inline def dao[E](using IntDaoSync[E]) : IntDaoSync[E]= summon
    inline def apply[E]( fromIdF : (id : Int,e : E) => E)(using Pool[java.sql.Connection]  ) :IntDaoSync[E] =
      given Sql[E] = Sql()
      new {
        val reqConstant: ReqConstant[E] = ReqConstant[E]()
        val compiledFunction: IdCompiledFunction[E] = IdCompiledFunction()
        def fromId(id : Int,e : E) : E = fromIdF(id,e)
      }


  trait IntDaoSync[E](using Pool[java.sql.Connection]  ,    Sql[E] ) extends  DaoOps.Sync[E,Int],Dao[E,Int]:
    def fromId(id : Int,e : E) : E
    val reqConstant: ReqConstant[E]
    inline def freeId : Int = maxId + 1
    def nextId(id : Int) : Int = id+1
    inline def save(e : E) : E =
      onPreStmt(reqConstant.insertString) {
        fillInsert(e, SimpleSql.thisPreStmt)
        val pre : PreparedStatement = SimpleSql.thisPreStmt
        SimpleSql.thisPreStmt.executeUpdate
        fromId(maxId,e)
      }
    inline def saveAll(es : Iterable[E]) : Iterable[E] =
      es.map(save(_))



end Dao
  
