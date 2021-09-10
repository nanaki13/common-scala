package bon.jo.datamodeler.model
import bon.jo.datamodeler.model.sql.SimpleSql.id

import java.time.LocalDateTime
object Model {
    case class User(@id id : Int,name : String,email : String = "")
    case class Room(@id id : Int,name : String)
    case class UserRoom(@id idUser : Int,@id idRoom : Int)
    case class UserUserMessage(@id idUserSource : Int,@id idUserDest : Int,@id idMessage : Int)
    case class Message(@id id : Int,content : String,time : LocalDateTime = LocalDateTime.now)
    case class RoomMessage(@id idRoom : Int,@id idMessage : Int)
    case class Event(id : Int,time : LocalDateTime = LocalDateTime.now)
    case class Groupe(id : Int, name : String = "Groupe 1")
    extension (u : User)
        inline def toRoom(r : Room) : UserRoom = UserRoom(u.id,r.id)

}
