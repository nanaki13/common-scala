package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.model.macros.SqlMacro

import java.sql.{PreparedStatement, ResultSet}


class IdCompiledFunction[E](
                             fillInsert: (E, PreparedStatement) => Unit,
                             val getIdFunction: (e: E) => Any,
                             readResultSet: (ResultSet, Int) => Seq[Any]
                           ) extends CompiledFunction(fillInsert,readResultSet)
object IdCompiledFunction:
  inline def apply[E](): IdCompiledFunction[E] =
    new IdCompiledFunction(
      SqlMacro.fillInsert[E],
      SqlMacro.uniqueIdValueAny[E],
      SqlMacro.readResultSet[E]
    )

class CompiledFunction[E](
                           val fillInsert: (E, PreparedStatement) => Unit,
                           val readResultSet: (ResultSet, Int) => Seq[Any]
)

object CompiledFunction:
  inline def apply[E](): CompiledFunction[E] =
    new CompiledFunction(
      SqlMacro.fillInsert[E],
      SqlMacro.readResultSet[E]
    )



