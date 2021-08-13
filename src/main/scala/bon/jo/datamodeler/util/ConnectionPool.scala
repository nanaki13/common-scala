package bon.jo.datamodeler.util

import bon.jo.datamodeler.model.Pool
import bon.jo.datamodeler.model.Pool.PoolImpl


import java.sql.{Connection, DriverManager, PreparedStatement}

object ConnectionPool:

  private def con(url : String) = () => DriverManager.getConnection(url)
  def apply(size : Int)(url : String,driverClass :String ):Pool[Connection]=
    Class.forName(driverClass)
    new PoolImpl[Connection](con(url), size) 


  type P[A] = Pool[Connection] ?=> A
  inline def pool:P[Pool[Connection]] = summon

