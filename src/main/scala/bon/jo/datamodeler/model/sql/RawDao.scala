package bon.jo.datamodeler.model.sql

trait RawDao[E]:
  type W[A]
  inline def sqlImpl(using  Sql[E]) = summon
  def wFactory[A](a: A): W[A]
  inline def max[T](inline fSel: E => T): W[T]
  inline def insert(e: E): W[Int]
  inline def insertAll(es: Iterable[E]): W[Int]
  inline def select(inline fSel: E => Any, value: Any)(using translate: Seq[Any] => E): W[Option[E]]
  inline def selectAll()(using translate: Seq[Any] => E): W[List[E]]
  inline def deleteAll(): W[Int]

  inline def delete(e: E, inline f: E => Any): W[Int]
  inline def join[B, IDB](
      inline fk: E => IDB
  )(using Dao.Sync[B, IDB], Seq[Any] => E, Seq[Any] => B): W[List[(E, B)]]
