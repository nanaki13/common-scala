package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.util.ConnectionPool.*
import bon.jo.datamodeler.util.Pool
import bon.jo.datamodeler.model.sql.SimpleSql
import bon.jo.datamodeler.model.sql.Sql
import bon.jo.datamodeler.model.macros.{GenMacro, SqlMacro}
import bon.jo.datamodeler.util.Utils.{/, UsingSb, writer}

import java.sql.Connection
import scala.util.Try
import bon.jo.datamodeler.util.Utils

import java.sql.ResultSet




case class ReqConstant(
                        insertString : String,
                        whereIdString : String,
                        selectAllString : String,
                        deleteIdString : String,
                        deleteString : String,
                        updateString : String)
object ReqConstant:
  type SQL[A,B] = Sql[A] ?=> B
  type Str[E] = Sql[E] ?=> String
  inline def sqlImpl[A] : SQL[A,Sql[A]] = summon[Sql[A]]
  inline def insertString[E]: Str[E] = {
    Utils.stringBuilder{
      sqlImpl.insert
      sqlImpl.value
      writer.toString
    }
  }

  inline def whereIdString[E] : Str[E] = {
    Utils.stringBuilder{
      /(" WHERE (")
      sqlImpl.idClause
      /(")")
      writer.toString
    }
  }
  inline def selectAllString[E] : Str[E]  =
    Utils.stringBuilder{
      sqlImpl.selectMe
      sqlImpl.from
      writer.toString
    }

  inline def deleteIdString[E] : Str[E] =
    Utils.stringBuilder{
      sqlImpl.delete
      /(" WHERE ")
      sqlImpl.idClause
    }

  inline def deleteString[E] : Str[E] =
    Utils.stringBuilder{
      sqlImpl.delete
    }

  inline def updateString[E] : Str[E] =
    Utils.stringBuilder{
      sqlImpl.update
    }

  inline def apply[E]()(using Sql[E] ) : ReqConstant =
    ReqConstant(insertString,whereIdString,selectAllString,deleteIdString,deleteString,updateString)


trait DaoSync[E,ID](using Pool[java.sql.Connection]  , Sql[E] ) extends Dao.Sync[E,ID]:

  val reqConstant : ReqConstant

  object EntityMethods:
    extension(e : E)
      inline def insert() : Int = DaoSync.this.insert(e)
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

  inline def fillInsert = SqlMacro.fillInsert[E]

  inline def insert(e: E) : Int = 
    onPreStmt(reqConstant.insertString){
      fillInsert(e,SimpleSql.thisPreStmt)
      SimpleSql.thisPreStmt.executeUpdate 
    }
  inline def insertAll(es: Iterable[E]) : Int = 
    onPreStmt(reqConstant.insertString){
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
      val readUser = SqlMacro.readResultSet[E]
      val res = SimpleSql.thisStmt.executeQuery(reqConstant.selectAllString)
      res.iterator(r => translate(readUser(r))).toList
      
    }
  
  inline def deleteAll():Int = 
    onStmt{
      SimpleSql.thisStmt.executeUpdate(reqConstant.deleteString)
    }

  inline def update(id : ID,e: E) : Int =
    println(e)
    val updateById = reqConstant.updateString + reqConstant.whereIdString
    println(updateById)
    onPreStmt(updateById){
      val nbCol = GenMacro.countFields[E]
      fillInsert(e,SimpleSql.thisPreStmt)
      SimpleSql.thisPreStmt.setObject(nbCol+1,id)
      SimpleSql.thisPreStmt.executeUpdate
    }

  inline def update(e: E) : Int =
    println(e)
    val updateById = reqConstant.updateString + reqConstant.whereIdString
    println(updateById)
    onPreStmt(updateById){
      val nbCol = GenMacro.countFields[E]
      fillInsert(e,SimpleSql.thisPreStmt)
      SimpleSql.thisPreStmt.setObject(nbCol+1,SqlMacro.uniqueIdValueAny(e))
      SimpleSql.thisPreStmt.executeUpdate
    }

  inline def delete( e : ID) : Int =
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
  inline def apply[E,ID](using Pool[java.sql.Connection]  , Sql[E] ) :DaoSync[E,ID] =
    new DaoSync[E,ID](){
    val reqConstant: ReqConstant = ReqConstant[E]()
  }

  object IntDaoSync :
    inline def apply[E](inline fromIdF : (id : Int,e : E) => E)(using Pool[java.sql.Connection]  , Sql[E] ) :IntDaoSync[E] =
      new IntDaoSync[E](){
        val reqConstant: ReqConstant = ReqConstant[E]()
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
  
