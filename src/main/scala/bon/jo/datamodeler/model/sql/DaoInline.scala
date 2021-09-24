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






trait DaoInline[E,ID](using Pool[java.sql.Connection], Sql[E] ) extends DaoOpsInline[E,ID] with  RawDaoInline[E,IdCompiledFunction[E]]:


  extension (e : E)
    inline def __id : Any = compiledFunction.getIdFunction(e)


  override inline def selectById(id: ID): W[Option[E]] =
    wFactory{
      println(reqConstant.selectById)
      onPreStmt(reqConstant.selectById){
        SimpleSql.thisPreStmt.setObject(1,id)
        val resultSet : ResultSet = SimpleSql.doQuery()
        if resultSet.next() then
          Some(compiledFunction.readResultSet(resultSet,0))
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
          compiledFunction.fillPreparedStatmentWithId(e,GenMacro.countFields[E] + 1,SimpleSql.thisPreStmt)
          SimpleSql.thisPreStmt.addBatch()
        SimpleSql.thisPreStmt.executeBatch.sum
      }
    }


  inline def update(id : ID,e: E) : W[Int] =
    wFactory {
      onPreStmt(reqConstant.updateById){
        val nbCol = GenMacro.countFields[E]
        compiledFunction.fillUpdate(e,SimpleSql.thisPreStmt)
        SimpleSql.thisPreStmt.setObject(GenMacro.countFields[E] + 1,id)
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




end DaoInline

object DaoInline:
  inline def dao[E](using RawDaoInline.Sync[E]) : RawDaoInline.Sync[E]= summon
  object EntityMethods:
    extension[E](e : E)(using RawDaoInline.Sync[E])
      inline def insertRaw()  = dao.insert(e)

  object IntEntityMethods:
    extension[E](e : E)(using IntDaoSyncInline[E])
      inline def insert()  = IntDaoSyncInline.dao.insert(e)
      inline def update()  = IntDaoSyncInline.dao.update(e)
      inline def delete()  = IntDaoSyncInline.dao.delete(e)
      inline def save()  = IntDaoSyncInline.dao.save(e)




  object IntDaoSyncInline :
    inline def dao[E](using IntDaoSyncInline[E]) : IntDaoSyncInline[E]= summon
    inline def apply[E]( fromIdF : (id : Int,e : E) => E)(using Pool[java.sql.Connection]  ) :IntDaoSyncInline[E] =
      given Sql[E] = Sql()
      new IntDaoSyncInline(fromIdF){
        val reqConstant: ReqConstant[E] = ReqConstant[E]()
        val compiledFunction: IdCompiledFunction[E] = IdCompiledFunction()

      }


  abstract class IntDaoSyncInline[E](override val fromId : (id : Int,e : E) => E)(using Pool[java.sql.Connection], Sql[E] ) extends  DaoOpsInline.Sync[E,Int](fromId),DaoInline[E,Int]







end DaoInline
  
