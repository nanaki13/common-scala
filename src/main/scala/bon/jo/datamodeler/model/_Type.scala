package bon.jo.datamodeler.model

enum _Type(name : _TypeName,size : Int):
  case Text(size : Int) extends _Type(_TypeName.Text,size)
  case Char(size : Int) extends _Type(_TypeName.Char,size)
  case Numeric(size : Int) extends _Type(_TypeName.Numeric,size)
  case FloatNumeric(size : Int) extends _Type(_TypeName.FloatNumeric,size)
  case Boolean extends _Type(_TypeName.Boolean,1)
  case Date extends _Type(_TypeName.Date,1)
  case Time extends _Type(_TypeName.Time,1)
  case DateTime extends _Type(_TypeName.DateTime,1)
object _Type:
  def from(name : _TypeName,size : Int = 10):_Type = name match
    case _TypeName.Text => _Type.Text(size)
    case _TypeName.Char => _Type.Char(size)
    case _TypeName.Numeric => _Type.Numeric(size)
    case _TypeName.FloatNumeric => _Type.FloatNumeric(size)
    case _TypeName.Boolean => _Type.Boolean
    case _TypeName.Date => _Type.Date
    case _TypeName.Time => _Type.Time
    case _TypeName.DateTime => _Type.DateTime
enum _TypeName:
  case Text
  case Char
  case Numeric
  case FloatNumeric
  case Boolean 
  case Date
  case Time
  case DateTime
