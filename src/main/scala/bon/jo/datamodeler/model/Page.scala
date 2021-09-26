package bon.jo.datamodeler.model

import bon.jo.datamodeler.model.ForFun.Fact

import scala.annotation.unchecked.uncheckedVariance

sealed trait Page:
  val size : Int
  val pageNumber : Int

object Page:



  given Fact[Response] with
    def apply[A](r : Response[A]) = r._data
    def apply[A](r : Iterable[A]): Response[A] = Response(r.toList,0,0)
  given toIteOnce[A]:  (Response[A] =>  IterableOnce[A]) with
    def apply(r : Response[A]) =  r._data
  
  case class Request(pageNumber : Int,
    size : Int
    ) extends Page:
      assert(size > 0,s"size must be > 0 ($size)")
  case class Response[+A] (
    _data : List[A]
    ,val pageNumber : Int,val pageCount : Int) extends Page:
    val size = _data.size










