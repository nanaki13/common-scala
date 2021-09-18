package bon.jo.datamodeler.model.sql



import bon.jo.datamodeler.util.ConnectionPool.onPreStmt
import bon.jo.datamodeler.util.{Pool, Utils}
import bon.jo.datamodeler.util.Utils.{/, writer}

import java.sql.{Connection, PreparedStatement}
import scala.concurrent.{ExecutionContext, Future}


trait DaoOpsInline[E,ID] extends RawDaoOpsInline[E]:

  inline def maxId :W[ID]
  inline def update(id : ID,e: E) : W[Int]
  inline def update(e: E) : W[Int]
  inline def insert(e: E): W[Int]
  inline def deleteById( e : ID) : W[Int]
  inline def delete(e: E): W[Int]
  inline def save(e : E) : W[E]
  inline def saveAll(es : Iterable[E]) : W[Iterable[E]]
  inline def selectById(id : ID) : W[Option[E]]



object DaoOpsInline :
  
  class CompiledDao[E,ID,V[_]](
                                val _maxId : () => V[ID],
                                val _update : (id : ID,e: E) => V[Int],
                                val _updateNoId : (e: E) => V[Int],
                                val _deleteById : ( e : ID) => V[Int],
                                val _delete : (e: E)=> V[Int],
                                val _save : (e : E) => V[E],
                                val _saveAll : (es : Iterable[E]) => V[Iterable[E]],
                                val _selectById : (id : ID) => V[Option[E]]
                         ):
    type W[A] = V[A]
    def maxId :W[ID] = _maxId()
    def update(id : ID,e: E) : W[Int]= _update(id,e)
    def update(e: E) : W[Int]=_updateNoId(e)
    def deleteById( e : ID) : W[Int]= _deleteById(e)
    def delete(e: E): W[Int]= _delete(e)
    def save(e : E) : W[E]= _save(e)
    def saveAll(es : Iterable[E]) : W[Iterable[E]]=_saveAll(es)
    def selectById(id : ID) : W[Option[E]] =_selectById(id)

  object Sync:

    inline def apply[E,ID]( fromId : (id : ID,e : E) => E)(using Pool[Connection]) : Sync[E,ID] =
      given Sql[E] = Sql()
      new  DaoInline[E,ID] with Sync[E,ID](fromId) {

        override val reqConstant : ReqConstant[E] = ReqConstant()
        override val compiledFunction : IdCompiledFunction[E]   = IdCompiledFunction()
      }
  trait Sync[E,ID](val fromId : (id : ID,e : E) => E)(using Pool[Connection]) extends DaoInline[E,ID]:
    type W[A] = A

    inline def save(e : E) : E =
      onPreStmt(reqConstant.insertString) {
        compiledFunction.fillInsert(e, SimpleSql.thisPreStmt)
        val pre : PreparedStatement = SimpleSql.thisPreStmt
        SimpleSql.thisPreStmt.executeUpdate
        fromId(maxId,e)
      }
    inline def saveAll(es : Iterable[E]) : Iterable[E] =
      es.map(save(_))
    override inline def wFactory[A](a: A): W[A] = a





  trait Async[E,ID](using ExecutionContext) extends DaoInline[E,ID]:
    type W[A] = Future[A]
    override inline def wFactory[A](a: A): W[A] = Future(a)



