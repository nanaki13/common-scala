package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.util.ConnectionPool.*
import bon.jo.datamodeler.util.Pool
import bon.jo.datamodeler.model.sql.SimpleSql
import bon.jo.datamodeler.model.macros.SqlMacro
import bon.jo.datamodeler.util.Utils.{writer,UsingSb,/}
import java.sql.Connection
import scala.util.Try
import bon.jo.datamodeler.util.Utils
import java.sql.ResultSet


trait BaseDao[E,ID]:
  inline def insertString : String
  inline def whereIdString : String
  inline def onStmt[A](f : StringBuilder ?=> SimpleSql.S[A]):A
  inline def onPreStmt[A](sql : String)(f : SimpleSql.SP[A]):A


trait DaoSync[E,ID](using Pool[java.sql.Connection]  , Sql[E] ) extends BaseDao[E,ID]:



  object EntityMethods:
    extension(e : E)
      inline def insert() : Int = DaoSync.this.insert(e)
  inline def sqlImpl(using  Sql[E]) = summon
 
  inline def insertString : String = {
    given StringBuilder = StringBuilder()
    sqlImpl.insert
    sqlImpl.value
    writer.toString
  }

  inline def whereIdString : String = {
    given StringBuilder = StringBuilder()
    sqlImpl.value
    writer.toString 
  }

  inline def onStmt[A](f : StringBuilder ?=> SimpleSql.S[A]):A =
    given  Connection =  pool.get 
    given StringBuilder = StringBuilder()
    val res = Try{SimpleSql.stmt{
      val rest = Try{
        val resL = f
        SimpleSql.thisStmt.close
        resL
      }
      SimpleSql.thisStmt.close
      rest.get
    }}
    pool.release
    res.get

  inline def onPreStmt[A](sql : String)(f : SimpleSql.SP[A]):A =
    given  Connection =  pool.get 
    val res = Try{SimpleSql.prepStmt(sql){
      val rest = Try{
        val resL = f
        SimpleSql.thisPreStmt.close
        resL
      }
      SimpleSql.thisPreStmt.close
      rest.get
    }}
    pool.release
    res.get
    
    
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

  inline def fillInsert = SqlMacro.fillInsert[E]

  inline def insert(e: E) : Int = 
    onPreStmt(insertString){
      fillInsert(e,SimpleSql.thisPreStmt)
      SimpleSql.thisPreStmt.executeUpdate 
    }
  inline def insertAll(es: Iterable[E]) : Int = 
    onPreStmt(insertString){
      for(e <- es)
        fillInsert(e,SimpleSql.thisPreStmt)
        SimpleSql.thisPreStmt.executeUpdate 
      SimpleSql.thisPreStmt.executeBatch.sum
    }

  inline def select(inline fSel : E => Any,value : Any)(using translate : List[Any] => E):Option[E] =  
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
        val readUser = SqlMacro.readResultSet[E]
        Some((translate(readUser(res))))
      else
        None
    }
  inline def selectAll()(using translate : List[Any] => E):List[E] =  

    onStmt{
      sqlImpl.selectMe 
      sqlImpl.from

      val sqlS = writer.toString
      
      val readUser = SqlMacro.readResultSet[E]
      val res = SimpleSql.thisStmt.executeQuery(sqlS)
      res.iterator(r => translate(readUser(r))).toList
      
    }
  
  inline def deleteAll():Int = 
    onStmt{
      sqlImpl.delete
      SimpleSql.thisStmt.executeUpdate(writer.toString) 
    }

  inline def delete( e : ID) : Int =
    onPreStmt(Utils.stringBuilder{
    sqlImpl.delete
    /(" WHERE ")
    sqlImpl.idClause}){
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
       
    
  extension (r : ResultSet)
    def iterator[A](read : ResultSet => A) : Iterator[A]=
      new Iterator[A]{
        
        def hasNext() :  Boolean = 
          val ret = r.next
          ret
        def next(): A = read(r)
      }
      
end DaoSync  

object DaoSync:
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
  
