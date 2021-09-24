package bon.jo.datamodeler.model

sealed trait Page:
  val size : Int
  val pageNumber : Int
case class Request(
  size : Int,
  pageNumber : Int) extends Page
case class Response[A,CC[_],C <: CC[A]] (
  size : Int,
  pageNumber : Int,pageCount : Int,
  _data : C
  ) extends Page

extension[A,CC[_],C <: CC[A]] (r : Response[A,CC,C])
  def map[B]() : Response[B,CC, CC[B]] = null


