package bon.jo.datamodeler.model

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import bon.jo.datamodeler.model.Model.User
import spray.json.DefaultJsonProtocol
import spray.json.DeserializationException
import spray.json.JsString
import spray.json.JsValue
import spray.json.RootJsonFormat

trait JsonSupport[T]{
  s :  SprayJsonSupport =>
  // import the default encoders for primitive types (Int, String, Lists etc)
  //import DefaultJsonProtocol._



  implicit val userFormat: RootJsonFormat[User] = jsonFormat3(User.apply)
}