package bon.jo.datamodeler.model.sql

import javax.sql.ConnectionPoolDataSource
import bon.jo.datamodeler.util.ConnectionPool

import java.sql.{Connection,Statement,PreparedStatement}
trait Transaction(using ConnectionPool.ConPool) {
  var connection : Option[Connection] = None
  var statement : Option[Statement] = None
  var preparedStatement : Option[PreparedStatement] = None
  
  def begin :Transaction = new{
    def con = {
      val c = ConnectionPool.pool.get 
      c.setAutoCommit(false)
      c
    }
    override val connection =
      Some(con)
  
  }
  def isBegan = connection != None
  def commit = connection.foreach(_.commit())
  def rollback = connection.foreach(_.rollback())
  
}
object Transaction:
  def onStmt[A](sql : String)(process : SimpleSql.S[A])(using tr : Transaction) : A =
    given Statement = tr.connection.map(_.createStatement()).get
    process
