package bon.jo.datamodeler.model.sql

import bon.jo.datamodeler.model.macros.SqlMacro

import java.sql.{PreparedStatement, ResultSet}

object CompiledFunction:
  inline def apply[E](): CompiledFunction[E] =
    new CompiledFunction(
      SqlMacro.fillInsert[E],
      SqlMacro.uniqueIdValueAny[E],
      SqlMacro.readResultSet[E]
    )

class CompiledFunction[E](
                           val fillInsert: (E, PreparedStatement) => Unit,
                           val getIdFunction: (e: E) => Any,
                           val readResultSet: (ResultSet, Int) => Seq[Any]
)
