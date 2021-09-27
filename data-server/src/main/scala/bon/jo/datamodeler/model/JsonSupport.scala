package bon.jo.datamodeler.model

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import bon.jo.datamodeler.model.Model.User
import spray.json.DefaultJsonProtocol
import spray.json.DeserializationException
import spray.json.JsString
import spray.json.JsValue
import spray.json.RootJsonFormat

trait JsonSupport[T]{

  given format: RootJsonFormat[T]
}

object JsonSupport:
  import DefaultJsonProtocol._
  given [T](using RootJsonFormat[T]) :  RootJsonFormat[Page.Response[T]] =
    jsonFormat[List[T],Int,Int,Page.Response[T]]((data,pageNumber,pageSize)=> Page.Response(data,pageNumber,pageSize),
      "pageNumber",
      "pageSize",
      "data")
