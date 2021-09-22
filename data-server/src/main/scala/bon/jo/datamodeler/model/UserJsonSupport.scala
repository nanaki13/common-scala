package bon.jo.datamodeler.model

import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import bon.jo.datamodeler.model.Model.User
import spray.json.{DefaultJsonProtocol, DeserializationException, JsString, JsValue, RootJsonFormat}

class UserJsonSupport extends JsonSupport[User] {

  // import the default encoders for primitive types (Int, String, Lists etc)
  import DefaultJsonProtocol._



  given format: RootJsonFormat[User] = jsonFormat3(User.apply)
}