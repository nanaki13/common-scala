package bon.jo.datamodeler.model
import bon.jo.datamodeler.model.sql.SimpleSql.id
object Model {
    case class User(@id id : Int,name : String, groupe : Int,email : String = "")
    case class Group(@id id : Int,name : String)
    opaque type UserGroup = (User,Group)
    object UserGroup:
      def apply(u : User,g : Group) : UserGroup = (u,g)
    extension (ug : UserGroup)
        def user : User = ug._1
        def group : Group = ug._2
}
