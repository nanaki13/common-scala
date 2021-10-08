package bon.jo.datamodeler.model.macros

case class FieldsClass(className : String, list: List[FieldInfo])
case class FieldInfo(name : String, className : String,parse : String => Any)
object FieldInfo:

  extension (self : FieldInfo)
    def parse(v : String) : Any =
      self.className match
        case "scala.Int" => v.toInt
