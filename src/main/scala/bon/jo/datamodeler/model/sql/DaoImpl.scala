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






trait DaoImpl[E,ID](using Pool[java.sql.Connection], Sql[E] ) extends Dao[E,ID] with  RawDaoImpl[E]:


  extension (e : E)
    inline def __id : Any = compiledFunction.getIdFunction(e)



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
      onPreStmt(reqConstant.updateById) {
        val nbCol = GenMacro.countFields[E]
        fillInsert(e, SimpleSql.thisPreStmt)
        SimpleSql.thisPreStmt.setObject(nbCol + 1, e.__id)
        SimpleSql.thisPreStmt.executeUpdate
      }
    }


  inline def delete(e: E) : W[Int] =
    wFactory {
      onPreStmt(reqConstant.deleteIdString) {
        SimpleSql.thisPreStmt.setObject(1, e.__id)
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
          fillInsert(e, SimpleSql.thisPreStmt)
          SimpleSql.thisPreStmt.setObject(nbCol + 1, e.__id)
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




end DaoImpl

object DaoImpl:
  inline def dao[E,ID](using DaoImpl[E,ID]) :DaoImpl[E,ID]= summon
  object EntityMethods:
    extension[E,ID](e : E)(using DaoImpl[E,ID])
      inline def insert()  = dao.insert(e)
      inline def update()  = dao.update(e)
      inline def delete()  = dao.delete(e)

    extension[E,ID](e : E)(using  IntDaoSync[E])
      inline def save() : Int = summon[IntDaoSync[E]].save(e)



  object IntDaoSync :
    inline def apply[E](inline fromIdF : (id : Int,e : E) => E)(using Pool[java.sql.Connection]  , Sql[E] ) :IntDaoSync[E] =
      new IntDaoSync[E](){
        val reqConstant: ReqConstant[E] = ReqConstant[E]()
        val compiledFunction: CompiledFunction[E] = CompiledFunction()
        def fromId(id : Int,e : E) : E = fromIdF(id,e)
      }


  trait IntDaoSync[E](using Pool[java.sql.Connection]  ,    Sql[E] ) extends DaoImpl[E,Int]:
    override type W[A] = A

    override inline def wFactory[A](a: A):  W[A] = a
    inline def freeId = maxId + 1
    def fromId(id : Int,e : E) : E 

    inline def save(e : E) : Int =
      insert(fromId(freeId,e))
    inline def saveAll(es : Iterable[E]) : Int =
      var currId = freeId
      insertAll(es.map{
        e =>
          val res = fromId(currId,e)
          currId+=1
          res
      })

      
end DaoImpl
  
