package bon.jo.datamodeler.model.sql
import Filtre.*
import bon.jo.datamodeler.model.macros.{GenMacro, SqlMacro}
object Filtre:
  opaque type BooleanFiltre = Filtre
  opaque type Exp = String
  opaque type BooleanExp = String
  opaque type Op = String
  opaque type BooleanOp = String
  val === : Op = "="
  val != : Op = "!="
  val > : Op = ">"
  val < : Op = "<"
  val OR : BooleanOp & Op = "OR"
  val LIKE : BooleanOp & Op = "LIKE"
  val AND : BooleanOp & Op = "AND"
  def alls = List(===,!=,<,>,OR,LIKE,AND)
  
  object Http{
    def http = List("eq","ne","lt","gt","or","like","and")
    given Map[String,Op] =
      assert(alls.size == http.size," have to have same size")
      http.zip(alls).toMap
  }
  object Op : 
    def apply(s : String)(using conf : Map[String,Op]) : Op = conf.getOrElse(s,throw new RuntimeException(s"not handle operator : $s"))
      
  val empty = Filtre.Empty
  def alias(s : String) = PrefixedFields(s,_)
  def composeBoolean(left: BooleanExp,op : BooleanOp,right: BooleanExp) : BooleanFiltre =  Compose(left.constantBoolean,op,right.constantBoolean)
  def composeExp(left: Exp,op : Op,right: Exp) : BooleanFiltre =  Compose(left.constantExp,op,right.constantExp)
  private def compose(left: Filtre,op :  Op ,right : Filtre) : Filtre = Compose(left,op,right)
  private def composeB(left: Filtre,op :  Op | BooleanOp ,right : Filtre) : BooleanFiltre = Compose(left,op,right)
  def apply(left : String,op: Op | BooleanOp,right: String ):Filtre = Compose(left.constantString,op,right.constantString)
  def apply(exp : String) : Exp & BooleanExp = exp
  extension (a : String)
    def exp : Constant = Constant(s"""'$a'""")
    def field : Constant = a.constantString
    def constantString : Constant = Constant(a)

  extension(a : BooleanExp)
    def &&(b : BooleanExp) : BooleanFiltre = composeBoolean(a,Filtre.AND,b)
    def &&(b : Filtre) : BooleanFiltre = composeB(a.constantBoolean,Filtre.AND,b)
    def ||(b : BooleanExp) : BooleanFiltre = composeBoolean(a,Filtre.OR,b)
    def ||(b : Filtre) : BooleanFiltre = composeB(a.constantBoolean,Filtre.OR,b)
    def constantBoolean : Constant = Constant(a)

  extension(a : Exp)
    def ===(b : Exp) : BooleanFiltre = composeExp(a,Filtre.===,b)
    def !=(b : Exp) : BooleanFiltre = composeExp(a,Filtre.!=,b)
    def >(b : Exp) : BooleanFiltre = composeExp(a,Filtre.>,b)
    def <(b : Exp) : BooleanFiltre = composeExp(a,Filtre.<,b)
    def like(b : Exp) : BooleanFiltre = composeExp(a,Filtre.LIKE,b)
    def constantExp : Constant = Constant(a)


  extension( a :BooleanFiltre)
    def &&(b : BooleanExp) : BooleanFiltre = compose(a,Filtre.AND,b.constantBoolean)
    def &&(b : Filtre) : BooleanFiltre = composeB(a,Filtre.AND,b)
    def ||(b : BooleanExp) : BooleanFiltre = compose(a,Filtre.OR,b.constantBoolean)
    def ||(b : Filtre) : BooleanFiltre = composeB(a,Filtre.OR,b)
    def value : String = a.value
    def typed[T](using d : Def[T]): Option[TypedFiltre[T]]= a.typed


case class ClassInfo[T](val fieldsName : List[String])
object Def:
  inline def apply[T](): Def[T] =
    
    given ClassInfo[T] = ClassInfo(SqlMacro.columnsNameList[T])
    new {}


trait Def[T](using ClassInfo[T]):
  inline def classInfo :  ClassInfo[T] = summon
  def validate(f : Filtre) : Boolean =
    f match {
      case Field(f) => classInfo.fieldsName.contains(f)
      case PrefixedFields(pref,f) => classInfo.fieldsName.contains(f)
      case Compose(a,_,b) => validate(a) && validate(b)
      case _ => true
    }

  def validate(f : Filtre,prefix: String) : Boolean =
    f match {
      case PrefixedFields(pref,f) if pref == prefix => classInfo.fieldsName.contains(f)
      case Compose(a,_,b) => validate(a) && validate(b)
      case _ => true
    }
case class TypedFiltre[T](val filtre: Filtre)
enum Filtre:
  def typed[T](using d : Def[T]): Option[TypedFiltre[T]] =
    if(d.validate(this)) then Some(TypedFiltre[T](this)) else None

  def value : String =
    this match {
      case Compose(left,op,right) =>  s"(${left.value} $op ${right.value})"
      case PrefixedFields(pref,field) =>  s"$pref.$field"
      case Field(field) =>  s"$field"
      case Constant(value) => value.toString
      case Empty => ""
    }
  //def exp : BooleanExp = value.exp
  case Compose(   left : Filtre,
                                 op : Op,
                                 right : Filtre)
  case PrefixedFields(pref : String,field : String )
  case Constant(valueAny : Any)
  case Field(valueAny : String)
  case Empty

  def ===(b : Filtre) : BooleanFiltre = composeB(this,Filtre.===,b)
  def !=(b : Filtre) : BooleanFiltre = composeB(this,Filtre.!=,b)
  def >(b : Filtre) : BooleanFiltre = composeB(this,Filtre.>,b)
  def <(b : Filtre) : BooleanFiltre = composeB(this,Filtre.<,b)
  def like(b : Filtre) : BooleanFiltre = composeB(this,Filtre.LIKE,b)
  inline def apply(op: Op) : Filtre => BooleanFiltre = composeB(this,op,_)




@main def testFiltre() =

   val a = alias("a")
   val testp = (( a("name") like "james".exp)  &&   a("active"))
   println(testp)
   println(testp.value)
