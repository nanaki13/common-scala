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

trait DaoSync[E,ID](using Pool[java.sql.Connection]  , Sql[E] ) extends Dao.Sync[E,ID]:

  val reqConstant : ReqConstant[E]
  val compiledFunction : CompiledFunction[E]

  extension (e : E)
    inline def __id : Any = compiledFunction.getIdFunction(e)

  inline def sqlImpl(using  Sql[E]) = summon

  inline def max[T](inline fSel : E => T):T =
    onStmt{
      sqlImpl.max(fSel)

      val resQ = SimpleSql.thisStmt.executeQuery(writer.toString)
      resQ.next
      val res = resQ.getObject(1)
      res.asInstanceOf[T]
    }

  inline def maxId : ID =
    onStmt{
      sqlImpl.maxId

      val resQ = SimpleSql.thisStmt.executeQuery(writer.toString)
      resQ.next
      val res = resQ.getObject(1)
      res.asInstanceOf[ID]
    }

  inline def fillInsert = compiledFunction.fillInsert

  inline def insert(e: E) : Int = 
    onPreStmt(reqConstant.insertString){
      fillInsert(e,SimpleSql.thisPreStmt)
      SimpleSql.thisPreStmt.executeUpdate 
    }

  inline def insertAll(es: Iterable[E]) : Int = 
    onPreStmt(reqConstant.insertString){
      for(e <- es)
        fillInsert(e,SimpleSql.thisPreStmt)
        SimpleSql.thisPreStmt.addBatch()
      SimpleSql.thisPreStmt.executeBatch.sum
    }

  inline def select(inline fSel : E => Any,value : Any)(using translate : Seq[Any] => E):Option[E] =
    val selSql = Utils.stringBuilder{
      sqlImpl.selectMe 
      sqlImpl.from
      sqlImpl.where(fSel) === "?"
    }

    onPreStmt(selSql){
      
      SimpleSql.thisPreStmt.setObject(1,value)
      val res = SimpleSql.thisPreStmt.executeQuery()
      if(res.next()) 
      then 

        Some((translate(compiledFunction.readResultSet(res,0))))
      else
        None
    }

  inline def selectById(value : ID)(using translate : Seq[Any] => E):Option[E] =
    onPreStmt(reqConstant.selectById){
      SimpleSql.thisPreStmt.setObject(1,value)
      val res = SimpleSql.thisPreStmt.executeQuery()
      if(res.next())
      then

        Some((translate(compiledFunction.readResultSet(res, 0))))
      else
        None
    }

  inline def selectAll()(using translate : Seq[Any] => E):List[E] =

    onStmt{
      val res = SimpleSql.thisStmt.executeQuery(reqConstant.selectAllString)
      res.iterator(r => translate(compiledFunction.readResultSet(r, 0))).toList
      
    }
  
  inline def deleteAll():Int = 
    onStmt{
      SimpleSql.thisStmt.executeUpdate(reqConstant.deleteString)
    }

  inline def update(id : ID,e: E) : Int =
    onPreStmt(reqConstant.updateById){
      val nbCol = GenMacro.countFields[E]
      fillInsert(e,SimpleSql.thisPreStmt)
      SimpleSql.thisPreStmt.setObject(nbCol+1,id)
      SimpleSql.thisPreStmt.executeUpdate
    }

  inline def update(e: E) : Int =
    onPreStmt(reqConstant.updateById){
      val nbCol = GenMacro.countFields[E]
      fillInsert(e,SimpleSql.thisPreStmt)
      SimpleSql.thisPreStmt.setObject(nbCol+1,e.__id)
      SimpleSql.thisPreStmt.executeUpdate
    }

  inline def delete(e: E) : Int =
    onPreStmt(reqConstant.deleteIdString){
      SimpleSql.thisPreStmt.setObject(1,e.__id)
      SimpleSql.thisPreStmt.executeUpdate
    }

  inline def updateAll(es:  Iterable[E]) : Int =
    onPreStmt(reqConstant.updateById){
      val nbCol = GenMacro.countFields[E]
      for(e <- es)
        fillInsert(e,SimpleSql.thisPreStmt)
        SimpleSql.thisPreStmt.setObject(nbCol+1,e.__id)
        SimpleSql.thisPreStmt.addBatch()
      SimpleSql.thisPreStmt.executeBatch.sum
    }

  inline def deleteById( e : ID) : Int =
    onPreStmt(reqConstant.deleteIdString){
      SimpleSql.thisPreStmt.setObject(1,e)
      SimpleSql.thisPreStmt.executeUpdate()
    }

  inline def delete( e : E,inline f : E => Any) : Int =
    val s = Utils.stringBuilder{
      sqlImpl.delete
      /(" WHERE ")
      sqlImpl.columnName(f)
      /(" = ? ")}
    println(s)
    onPreStmt(s){
      SimpleSql.thisPreStmt.setObject(1,f(e))
      SimpleSql.thisPreStmt.executeUpdate()
    }

  inline def join[B,IDB](inline fk : E => IDB)(using Dao.Sync[B,IDB],Seq[Any] => E,Seq[Any] => B):W[List[(E,B)]] =
    val otherDao : Dao.Sync[B,IDB] = summon[ Dao.Sync[B,IDB]]
    val l : Seq[Any] => E = summon[Seq[Any] => E]
    val r : Seq[Any] => B = summon[Seq[Any] => B]
    val join = ReqConstant.selectJoin(reqConstant,otherDao.reqConstant,GenMacro.fieldSelection(fk)._2,SqlMacro.uniqueIdString[B])

    def readResulsetJoin(resultSet: ResultSet):(E,B) =
      (l(compiledFunction.readResultSet(resultSet,0)),
      r(otherDao.compiledFunction.readResultSet(resultSet,reqConstant.columns.size)))

    onStmt{
      val res = SimpleSql.thisStmt.executeQuery(join)
      res.iterator(readResulsetJoin).toList

    }

       
    
  extension (r : ResultSet)
    def iterator[A](read : ResultSet => A) : Iterator[A]=
      new Iterator[A]{
        
        inline def hasNext() :  Boolean =
          val ret = r.next
          ret
        inline def next(): A = read(r)
      }
      
end DaoSync  

object DaoSync:
  inline def dao[E,ID](using DaoSync[E,ID]) :DaoSync[E,ID]= summon
  object EntityMethods:
    extension[E,ID](e : E)(using  DaoSync[E,ID])
      inline def insert() : Int = dao.insert(e)
      inline def update() : Int = dao.update(e)
      inline def delete() : Int = dao.delete(e)

    extension[E,ID](e : E)(using  IntDaoSync[E])
      inline def save() : Int = summon[IntDaoSync[E]].save(e)
  class BasDaoSync[E,ID](   freeId :()=> ID , fromIdF : (id : ID,e : E) => E)(using Pool[java.sql.Connection]  , Sql[E],CompiledFunction[E],ReqConstant[E] ) extends  DaoSync[E,ID]:

    val reqConstant: ReqConstant[E] = summon[ReqConstant[E]]
    val compiledFunction: CompiledFunction[E] = summon[CompiledFunction[E]]
    inline def save(e : E) : Int =
      insert(fromIdF(freeId(),e))

    inline def saveAll(es : Iterable[E]) : Int =

      insertAll(es.map{
        e =>
          val res = fromIdF(freeId(),e)

          res
      })
  inline def apply[E,ID]( inline  freeId :()=> ID ,inline fromIdF : (id : ID,e : E) => E)(using Pool[java.sql.Connection]  , Sql[E],CompiledFunction[E],ReqConstant[E] ) :DaoSync[E,ID] =
    BasDaoSync(freeId,fromIdF)

  object IntDaoSync :
    inline def apply[E](inline fromIdF : (id : Int,e : E) => E)(using Pool[java.sql.Connection]  , Sql[E] ) :IntDaoSync[E] =
      new IntDaoSync[E](){
        val reqConstant: ReqConstant[E] = ReqConstant[E]()
        val compiledFunction: CompiledFunction[E] = CompiledFunction()
        def fromId(id : Int,e : E) : E = fromIdF(id,e)
      }
  trait IntDaoSync[E](using Pool[java.sql.Connection]  ,    Sql[E] ) extends DaoSync[E,Int]:
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

      
end DaoSync 
  
