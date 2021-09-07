package bon.jo.datamodeler.model
import bon.jo.datamodeler.model.sql.SimpleSql.id

import java.time.LocalDateTime
object Model {
    case class User(@id id : Int,name : String,email : String = "")
    case class Room(@id id : Int,name : String)
    case class UserRoom(@id idUser : Int,@id idRoom : Int)
    case class Event(id : Int,time : LocalDateTime = LocalDateTime.now)
    case class Groupe(id : Int, name : String = "Groupe 1")

}
