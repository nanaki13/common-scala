package bon.jo.datamodeler.model
import bon.jo.datamodeler.model.sql.SimpleSql.id
object Model {
    case class User(@id id : Int,name : String,email : String = "")
    case class Room(@id id : Int,name : String)
    case class UserRoom(@id idUser : Int,@id idRoom : Int)

}
