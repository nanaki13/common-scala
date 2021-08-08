package bon.jo.datamodeler.model

import bon.jo.datamodeler.model.SimpleSql.User
import bon.jo.datamodeler.model.SqlWriter.{UsingSb, writer}
import bon.jo.datamodeler.model.`macro`.TestMacro

object SqlWriter:
  type UsingSb[A] = StringBuilder ?=> A
  def writer: UsingSb[StringBuilder] = summon
  case class SelectCase(value: String)(using StringBuilder):

    writer.append("SELECT ")
    writer.append(value)

    def fromCase(f: FromCase): UsingSb[SelectCase] =
      writer.append(" FROM ")
      writer.append(f.value)
      this



    def whereCase(w: Where): UsingSb[SelectCase] =
      writer.append(" WHERE ")
      writer.append(w.value)
      this

  inline def from[T]: UsingSb[Unit] =
    writer.append(" FROM ")
    writer.append(TestMacro.tableName[T].name)
  inline def where[T](inline f: T => Any): UsingSb[Unit] =
    writer.append(" WHERE ")
    writer.append(TestMacro.fieldSelection[T](f)._2)

  case class FromCase(value: Any)
  case class Where(value: Any)

  inline def select[T]: UsingSb[Unit] =
    writer.append("SELECT ")
    writer.append(TestMacro.columnsName[T])
  inline def select[T,C]: UsingSb[Unit] =
    writer.append("SELECT ")
    writer.append(TestMacro.columnsName[T])
    writer.append(TestMacro.columnsName[C])






  object ~~




trait Sql[A]:
  inline def selectMe : UsingSb[Sql[A]] =
    SqlWriter.select[A]
    this
  inline def select[B] : UsingSb[Sql[A]] =
    writer.append(',')
    writer.append(TestMacro.columnsName[B])
    this
  inline def from : UsingSb[Sql[A]] =
    SqlWriter.from[A]
    this
  inline def where(inline f: A => Any) : UsingSb[Sql[A]] =
    SqlWriter.where[A](f)
    this

  inline def or(inline f: A => Any) : UsingSb[Sql[A]] =
    writer.append(" OR ")
    writer.append(TestMacro.fieldSelection[A](f)._2)
    this

  inline def join[B](inline f: A => Any,inline g: B => Any): UsingSb[Sql[A]] =
    writer.append(" JOIN ")
    writer.append(TestMacro.tableName[B].name)
    writer.append(" ON ")
    writer.append(TestMacro.fieldSelection[A](f)._2)
    writer.append(" = ")
    writer.append(TestMacro.fieldSelection[B](g)._2)
    this


  def ===(b: Any): UsingSb[Sql[A]] =
    writer.append(s" = $b")
    this


case class Group(id : Int,name : String)

object T extends App:

  object UserSql extends Sql[User]
  import UserSql.*
  given StringBuilder = StringBuilder()

  selectMe
  select[Group]
  from
  join[Group](_.groupe,_.id)
  where(z => z.groupe) === 1
  or(_.groupe)  === 2

  println(writer)





