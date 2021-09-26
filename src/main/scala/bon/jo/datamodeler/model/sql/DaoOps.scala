package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.model.{Page, sql}
import bon.jo.datamodeler.util.Pool

import java.sql.Connection
import scala.concurrent.{ExecutionContext, Future}


trait DaoOps[E,ID] extends RawDaoOps[E]:
     def maxId :W[ID]
     def update(id : ID,e: E) : W[Int]
     def update(e: E) : W[Int]
     def deleteById( e : ID) : W[Int]
     def delete(e: E): W[Int]
     def save(e : E) : W[E]
     def saveAll(es : Iterable[E]) : W[Iterable[E]]
     def selectById(id : ID) : W[Option[E]]

object DaoOps:
    abstract  class Impl[E,ID](
                    val __insert : (e: E) =>  Int,
                    val __insertAll : (es: Iterable[E]) =>  Int,
                    val __selectAll : () =>  List[E],
                    val __selectAllPaged : (page : Page) =>  Page.Response[E],
                    val __deleteAll : () =>  Int,
                    val _maxId : () => ID,
                    val _updateId : (id : ID,e: E) => Int,
                    val _update : (e: E) => Int,
                    val _deleteById : ( e : ID) => Int,
                    val _delete : (e: E)=> Int ,
                    val _save : (e : E) => E,
                    val  _saveAll : (es : Iterable[E]) => Iterable[E] ,
                    val  _selectById : (id : ID) => Option[E]

                 )(using d :  DaoOpsInline.Sync[E,ID]) extends DaoOps[E,ID] with RawDaoOps.Impl[E](__insert,__insertAll,__selectAll,__selectAllPaged,__deleteAll):
        def maxId :W[ID]     = wFactory( _maxId()  )
        def update(id : ID,e: E) : W[Int]  = wFactory( _updateId(id,e)  )
        def update(e: E) : W[Int] = wFactory( _update(e)  )
        def deleteById( e : ID) : W[Int]   = wFactory( _deleteById(e)  )
        def delete(e: E): W[Int]    = wFactory( _delete(e)  )
        def save(e : E) : W[E]  = wFactory( _save(e)  )
        def saveAll(es : Iterable[E]) : W[Iterable[E]]   = wFactory( _saveAll(es)  )
        def selectById(id : ID) : W[Option[E]]  = wFactory( _selectById(id)  )
    class Sync[E,ID](
                      _insert : (e: E) =>  Int,
                      _insertAll : (es: Iterable[E]) =>  Int,
                      _selectAll : () =>  List[E],
                      _selectAllPaged : (page : Page) =>  Page.Response[E],
                      _deleteAll : () =>  Int,
                      _maxId : () => ID,
                      _updateId : (id : ID,e: E) => Int,
                      _update : (e: E) => Int,
                      _deleteById : ( e : ID) => Int,
                      _delete : (e: E)=> Int ,
                      _save : (e : E) => E,
                      _saveAll : (es : Iterable[E]) => Iterable[E] ,
                      _selectById : (id : ID) => Option[E]

                    )(using d :  DaoOpsInline.Sync[E,ID]) extends  Impl[E,ID]( _insert : (e: E) =>  Int,
      _insertAll,
      _selectAll,
      _selectAllPaged,
      _deleteAll ,
      _maxId,
      _updateId ,
      _update ,
      _deleteById ,
      _delete ,
      _save ,
      _saveAll ,
      _selectById ) with  DaoOps[E,ID]:
        override type W[A] = A
        override inline def wFactory[A](a : A) = a
    object Sync:
      inline def apply[E,ID](using d :  DaoOpsInline.Sync[E,ID]) :  Sync[E,ID]
            =  new Sync[E,ID](
            d.insert(_),
            d.insertAll(_),
            () => d.selectAll(),
            d.selectAll(_),
            () =>d.deleteAll(),
            () =>d.maxId,
            d.update(_,_),
            d.update(_),
            d.deleteById(_),d.delete(_),d.save(_),d.saveAll(_),d.selectById(_)
        )
      inline def fromPool[E,ID]( fromId : (id : ID,e : E) => E)(using Pool[Connection]) :  Sync[E,ID]  =
        given d : DaoOpsInline.Sync[E,ID] =  DaoOpsInline.Sync[E,ID](fromId)
        new Sync[E,ID](
          d.insert(_),
          d.insertAll(_),
          () => d.selectAll(),
          d.selectAll(_),
          () =>d.deleteAll(),
          () =>d.maxId,
          d.update(_,_),
          d.update(_),
          d.deleteById(_),d.delete(_),d.save(_),d.saveAll(_),d.selectById(_))






