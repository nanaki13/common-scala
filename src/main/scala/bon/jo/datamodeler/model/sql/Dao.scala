package bon.jo.datamodeler.model.sql



import bon.jo.datamodeler.util.Utils
import bon.jo.datamodeler.util.Utils.{/, writer}

import scala.concurrent.{ExecutionContext, Future}

trait Dao[E,ID]:
  type W[A]
  inline def wFactory[A](a: A) : W[A]

  inline def max[T](inline fSel : E => T):W[T]

  inline def maxId :W[ID]

  inline def insert(e: E) :W[Int]

  inline def insertAll(es: Iterable[E]) : W[Int]

  inline def select(inline fSel : E => Any,value : Any)(using translate : Seq[Any] => E):W[Option[E]]

  inline def selectAll()(using translate : Seq[Any] => E):W[List[E]]

  inline def deleteAll():W[Int]

  inline def update(id : ID,e: E) : W[Int]

  inline def update(e: E) : W[Int]

  inline def deleteById( e : ID) : W[Int]

  inline def delete( e : E) : W[Int]

  inline def delete( e : E,inline f : E => Any) : W[Int]

  inline def join[B,IDB](inline fk : E => IDB)(using Dao.Sync[B,IDB],Seq[Any] => E,Seq[Any] => B):W[List[(E,B)]]


object Dao :
  trait Sync[E,ID] extends Dao[E,ID]:
    type W[A] = A
    val reqConstant : ReqConstant[E]
    val compiledFunction : CompiledFunction[E]
    override inline def wFactory[A](a: A): W[A] = a

  trait Async[E,ID](using ExecutionContext) extends Dao[E,ID]:
    type W[A] = Future[A]
    override inline def wFactory[A](a: A): W[A] = Future(a)
  trait DelegateAsync[E,ID](using Sync[E,ID], ExecutionContext) extends Async[E,ID]:
    inline def sync(using Sync[E,ID]) = summon
    inline def max[T](inline fSel : E => T):W[T] = Future(sync.max[T](fSel))
  
    inline def maxId :W[ID] = Future(sync.maxId)
  
    inline def insert(e: E) :W[Int] = Future(sync.insert(e))
  
    inline def insertAll(es: Iterable[E]) : W[Int] = Future(sync.insertAll(es))
  
    inline def select(inline fSel : E => Any,value : Any)(using translate : Seq[Any] => E):W[Option[E]]= Future(sync.select( fSel ,value))
  
    inline def selectAll()(using translate : Seq[Any] => E):W[List[E]]
  
    inline def deleteAll():W[Int]
  
    inline def update(id : ID,e: E) : W[Int]
  
    inline def update(e: E) : W[Int]
  
    inline def deleteById( e : ID) : W[Int]

    inline def delete( e : E) : W[Int]
  
    inline def delete( e : E,inline f : E => Any) : W[Int]


