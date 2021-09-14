package bon.jo.datamodeler.model
import bon.jo.datamodeler.model.sql.SimpleSql.id

import java.time.LocalDateTime
object Model {
    case class User(@id id : Int = null.asInstanceOf[Int],name : String,email : String = "")
    case class Room(@id id : Int = null.asInstanceOf[Int],name : String)
    case class UserRoom(@id idUser : Int,@id idRoom : Int)
    case class UserUserMessage(@id idUserSource : Int,@id idUserDest : Int,@id idMessage : Int)
    case class Message(@id id : Int = 0,content : String,time : LocalDateTime = LocalDateTime.now)
    case class RoomMessage(@id idUserSource  : Int, @id idRoom : Int,@id idMessage : Int)
    case class Event(id : Int,time : LocalDateTime = LocalDateTime.now)
    case class Groupe(id : Int, name : String = "Groupe 1")
    extension (u : User)
        inline def toRoom(r : Room) : UserRoom = UserRoom(u.id,r.id)

    extension (m : Message)
        inline def to(dest : Room ) : RoomMessage =
            RoomMessage(0, dest.id,m.id)
        inline def to(dest : User) : UserUserMessage =
            UserUserMessage(0,dest.id,m.id)

        inline def toRoom(dest : Int ) : RoomMessage =
            RoomMessage(0, dest,m.id)
        inline def toUser(dest : Int) : UserUserMessage =
            UserUserMessage(0,dest,m.id)

}
