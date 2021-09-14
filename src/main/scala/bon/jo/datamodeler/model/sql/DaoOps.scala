package bon.jo.datamodeler.model.sql



import bon.jo.datamodeler.util.Utils
import bon.jo.datamodeler.util.Utils.{/, writer}

import scala.concurrent.{ExecutionContext, Future}


trait DaoOps[E,ID] extends RawDaoOps[E]:
  type W[A]
   inline def maxId :W[ID]
   inline def update(id : ID,e: E) : W[Int]
   inline def update(e: E) : W[Int]
   inline def deleteById( e : ID) : W[Int]
   inline def delete(e: E): W[Int]
   inline def save(e : E) : W[E]
   inline def saveAll(es : Iterable[E]) : W[Iterable[E]]
   inline def selectById(id : ID) : W[Option[E]]



object DaoOps :
  trait Sync[E,ID] extends DaoOps[E,ID]:
    type W[A] = A
    val reqConstant : ReqConstant[E]
    val compiledFunction : CompiledFunction[E]
    override inline def wFactory[A](a: A): W[A] = a

  trait Async[E,ID](using ExecutionContext) extends Dao[E,ID]:
    type W[A] = Future[A]
    override inline def wFactory[A](a: A): W[A] = Future(a)



