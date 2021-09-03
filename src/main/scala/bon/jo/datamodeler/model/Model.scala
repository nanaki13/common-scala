package bon.jo.datamodeler.model
import bon.jo.datamodeler.model.sql.SimpleSql.id
object Model {
    case class User(@id id : Int,name : String, groupe : Int,email : String = "")
}
