package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.util.ConnectionPool.*
import bon.jo.datamodeler.util.Pool
import bon.jo.datamodeler.model.sql.SimpleSql
import bon.jo.datamodeler.model.macros.SqlMacro
import bon.jo.datamodeler.util.Utils.{writer,UsingSb}
import java.sql.Connection
import scala.util.Try
import bon.jo.datamodeler.util.Utils
import java.sql.ResultSet

trait DaoSync[E,ID](using Pool[java.sql.Connection]  ):
  
  val sql : Sql[E] = Sql()
  inline def insertString = {
    given StringBuilder = StringBuilder()
    sql.insert
    sql.value
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
    
    
  inline def max(inline fSel : E => Any) =
    onStmt{
      sql.max(fSel)
      println(writer.toString)
      val resQ = SimpleSql.thisStmt.executeQuery(writer.toString)
      resQ.next
      val res = resQ.getObject(1)
      res
    }

  inline def fillInsert = SqlMacro.fillInsert[E]

  inline def insert(e: E) : Int = 
    onPreStmt(insertString){
      fillInsert(e,SimpleSql.thisPreStmt)
      SimpleSql.thisPreStmt.executeUpdate 
    }

  inline def select(inline fSel : E => Any,value : Any)(using translate : List[Any] => E):Option[E] =  
    val selSql = Utils.stringBuilder{
      sql.selectMe 
      sql.from
      sql.where(fSel) === "?"
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
      sql.selectMe 
      sql.from

      val sqlS = writer.toString
      println(sqlS)
      val readUser = SqlMacro.readResultSet[E]
      val res = SimpleSql.thisStmt.executeQuery(sqlS)
      res.iterator(r => translate(readUser(r))).toList
      
    }
    
  extension (r : ResultSet)
    def iterator[A](read : ResultSet => A) : Iterator[A]=
      new Iterator[A]{
        
        def hasNext() :  Boolean = 
          val ret = r.next
          println(ret)
          ret
        def next(): A = read(r)
      }
      
end DaoSync  
  
