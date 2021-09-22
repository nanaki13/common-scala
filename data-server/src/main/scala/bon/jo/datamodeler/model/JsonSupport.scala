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