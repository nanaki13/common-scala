package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.model.sql

import scala.concurrent.{ExecutionContext, Future}


trait RawDaoOps[E]:
    type W[A]
    def sqlImpl(using  Sql[E]) = summon
    def wFactory[A](a: A): W[A]
   // inline def max[T](inline fSel: E => T): W[T]
    def insert(e: E): W[Int]
    def insertAll(es: Iterable[E]): W[Int]
  // inline def select(inline fSel: E => Any, value: Any): W[Option[E]]
    def selectAll(): W[List[E]]
    def deleteAll(): W[Int]

  // inline def delete(e: E, inline f: E => Any): W[Int]
  /*  inline def join[B, IDB](
      inline fk: E => IDB
  )(using DaoOpsInline.Sync[B, IDB]): W[List[(E, B)]]*/

object RawDaoOps:

  class Sync[E](
                  _insert : (e: E) =>  Int,
                  _insertAll : (es: Iterable[E]) =>  Int,
                  _selectAll : () =>  List[E],
                  _deleteAll : () =>  Int
               )(using RawDaoOpsInline.Sync[E]) extends Impl[E](_insert,_insertAll,_selectAll,_deleteAll):
    override type W[A] = A
    override inline def wFactory[A](a :A) = a
  class Async[E](
                   _insert : (e: E) =>  Int,
                   _insertAll : (es: Iterable[E]) =>  Int,
                   _selectAll : () =>  List[E],
                   _deleteAll : () =>  Int
                 )(using RawDaoOpsInline.Sync[E], ExecutionContext) extends Impl[E](_insert,_insertAll,_selectAll,_deleteAll):
    override type W[A] = Future[A]
    override inline def wFactory[A](a :A) = Future(a)
  trait Impl[E](
    val _insert : (e: E) =>  Int,
    val _insertAll : (es: Iterable[E]) =>  Int,
    val _selectAll : () =>  List[E],
    val _deleteAll : () =>  Int

                 ) extends RawDaoOps[E]:


    //inline def sub : RawDaoOpsInline.Sync[E] = summon
    //  def wFactory[A](a: A): W[A]
    //inline def max[T](inline fSel: E => T): W[T]  = wFactory(sub.max(fSel))
    def insert(e: E): W[Int] = wFactory(_insert(e))
    def insertAll(es: Iterable[E]): W[Int] =wFactory( _insertAll(es))
    //inline def select(inline fSel: E => Any, value: Any): W[Option[E]] = wFactory(sub.select(fSel,value))
    def selectAll(): W[List[E]] = wFactory(_selectAll())
    def deleteAll(): W[Int] = wFactory(_deleteAll())

    // inline def delete(e: E, inline f: E => Any): W[Int]  = wFactory(sub.delete(e,f))
   /* inline def join[B, IDB](
                    inline fk: E => IDB
                  )(using DaoOpsInline.Sync[B, IDB]): W[List[(E, B)]]   = wFactory(sub.join(fk))*/

