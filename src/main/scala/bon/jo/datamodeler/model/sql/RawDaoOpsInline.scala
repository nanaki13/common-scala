package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.model.Page

import scala.concurrent.{ExecutionContext, Future}

trait RawDaoOpsInline[E]:
  type W[A]
  inline def sqlImpl(using  Sql[E]) = summon
  inline def wFactory[A](a: A): W[A]
  inline def max[T](inline fSel: E => T): W[T]
  inline def insert(e: E): W[Int]
  inline def insertAll(es: Iterable[E]): W[Int]
  inline def select(inline fSel: E => Any, value: Any): W[Option[E]]
  inline def selectAll(): W[List[E]]
  inline def selectAll(page : Page): W[Page.Response[E]]
  inline def deleteAll(): W[Int]

  inline def delete(e: E, inline f: E => Any): W[Int]
  inline def join[B, IDB](
      inline fk: E => IDB
  )(using DaoOpsInline.Sync[B, IDB]): W[List[(E, B)]]
object RawDaoOpsInline:
  trait Sync[E] extends RawDaoOpsInline[E]:
    type W[A] = A
    val reqConstant : ReqConstant[E]
    val compiledFunction : CompiledFunction[E]
    override inline def wFactory[A](a: A): W[A] = a

  trait Async[E](using ExecutionContext) extends RawDaoOpsInline[E]:
    type W[A] = Future[A]
    override inline def wFactory[A](a: A): W[A] = Future(a)
