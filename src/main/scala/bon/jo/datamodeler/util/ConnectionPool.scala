package bon.jo.datamodeler.util

import bon.jo.datamodeler.model.sql.SimpleSql
import bon.jo.datamodeler.util.Pool
import bon.jo.datamodeler.util.Pool.PoolImpl

import java.sql.{Connection, DriverManager, PreparedStatement}
import scala.util.Try

object ConnectionPool:

  type ConPool = Pool[Connection]
  type Pooled[A] = ConPool ?=> A
  private def con(url : String) = () => DriverManager.getConnection(url)
  def apply(size : Int)(url : String,driverClass :String ):Pool[Connection]=
    Class.forName(driverClass)
    new PoolImpl[Connection](con(url), size) 


  type P[A] = Pool[Connection] ?=> A
  inline def pool:P[Pool[Connection]] = summon

  inline def onStmtDo(f : SimpleSql.S[Unit] ) : Pooled[Unit] =
    given  Connection =  pool.get
    Try{SimpleSql.stmt[Unit]{
      f
      SimpleSql.thisStmt.close
    }}
    pool.release
  inline def onStmt[A](f : StringBuilder ?=> SimpleSql.S[A]) : Pooled[A] =
    given  Connection =  pool.get
    given StringBuilder = StringBuilder()
    val res = Try{SimpleSql.stmt{
      val rest = Try{
        val resL = f
        SimpleSql.thisStmt.close
        resL
      }
      SimpleSql.thisStmt.close
      rest.get
    }}
    pool.release
    res.get

  inline def onPreStmt[A](sql : String)(f : SimpleSql.SP[A]):Pooled[A] =
    given  Connection =  pool.get
    val res = Try{SimpleSql.prepStmt(sql){
      val rest = Try{
        val resL = f
        SimpleSql.thisPreStmt.close
        resL
      }
      SimpleSql.thisPreStmt.close
      rest.get
    }}
    pool.release
    res.get

  extension (p :  Pool[Connection])
    def closeAll():Unit = 
     
        p.toAll( con =>
          given Connection = con
          try
            con.close
            p.release
          catch 
            case e : java.sql.SQLException => println(e)
          )
     

