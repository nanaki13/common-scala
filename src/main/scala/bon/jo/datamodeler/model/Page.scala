package bon.jo.datamodeler.model

import bon.jo.datamodeler.model.ForFun.Fact

import scala.annotation.unchecked.uncheckedVariance
import scala.concurrent.{ExecutionContext, Future}

sealed trait Page:
  val size : Int
  val pageNumber : Int

object Page:

  object empty extends Response[Nothing](Nil,0,0)

  def apply(pageNumberOption : Option[Int],pageSizeOption : Option[Int]) : Page.Request =
    Page.Request(pageNumberOption.getOrElse(0),pageSizeOption.getOrElse(50))

  given Fact[Response] with
    def apply[A](r : Response[A]) = r._data
    def apply[A](r : Iterable[A]): Response[A] = Response(r.toList,0,0)
  given asIterable[A]:  (Response[A] =>  IterableOnce[A]) with
    def apply(r : Response[A]) =  r._data
  
  case class Request(pageNumber : Int,
    size : Int
    ) extends Page:
      assert(size > 0,s"size must be > 0 ($size)")
  case class Response[+A] (
    _data : List[A]
    ,val pageNumber : Int,val pageCount : Int) extends Page:
    val size = _data.size
  type PageFlow[A] =(Page.Request )=> Future[Page.Response[A]]
  def pageFlow[A](using  PageFlow[A]) : PageFlow[A] = summon
  def emptyPage[A] :  Future[Page.Response[A]] = Future.successful(empty)
  
  
  extension (request : Page.Request)
    def first[A]()(using PageFlow[A]): Future[Page.Response[A]]  = pageFlow(request)
    def readAll[A](using PageFlow[A], ExecutionContext) : Future[Seq[Page.Response[A]]] =
      val firstPage = first[A]()
      (for{
        i <- firstPage
        
      } yield {
        val ff : Seq[Future[Response[A]]] =
          Future.successful(i) +: (for(cnt <- 1 to i.pageCount-1) yield{
          pageFlow[A](request.copy(cnt,request.size))
        })
        Future.sequence(ff)
      }).flatten
      
      
    def next[A](p : Page.Response[A])(using PageFlow[A]) : Future[Page.Response[A]] =
      if(p.pageCount+1 != p.pageNumber)
        pageFlow(request.copy(pageNumber = p.pageNumber+1))
      else
        emptyPage











