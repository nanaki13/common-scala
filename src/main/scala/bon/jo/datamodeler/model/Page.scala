package bon.jo.datamodeler.model

import scala.annotation.unchecked.uncheckedVariance

sealed trait Page:
  val size : Int
  val pageNumber : Int

object Page:
  

  case class Request(
    size : Int,
    pageNumber : Int) extends Page
  case class Response[+A,+CC[_] <: Iterable[_],+C] (
    size : Int,
    pageNumber : Int,pageCount : Int,
    _data : C
    ) extends Page

  type R[A,C[_] <: Iterable[_]] = Response[A ,C,C[A]]
  type PageList[A] = R[A,List]
  object PageList:
    def apply[A](pageNumber : Int,pageCount : Int,l : List[A]) :  PageList[A] = 
      Response(l.size,pageNumber,pageCount,l)
  extension[A,T[_] <: Iterable[_]] (r : R[A,T])

    def empty[B] :  R[B,T] = Response(0,
      r.pageNumber,r.pageCount,
      r._data.empty.asInstanceOf[T[B]])
    def ++(o : R[A,T]):  R[A,T] =
      Response(r.size + o.size,r.pageNumber,r.pageCount,(r._data ++ o._data).asInstanceOf[T[A]])

    def flatMap[B](f : A => R[B,T]): R[B,T] =
      r._data
        .map[R[B,T]](
          f.asInstanceOf[(Any => R[B,T])])
        .foldLeft[R[B,T]](empty)((a,b) =>  ( a++ b).asInstanceOf[R[B,T]])
    def map[B](f : A => B): Response[B,T,T[B]] =
      val d : T[A] = r._data
      Response[B,T,T[B]](r.size,
        r.pageNumber,
        r.pageCount,
          d.map[B](f.asInstanceOf[(Any => B)]).asInstanceOf[T[B]])




