package bon.jo.datamodeler.util

import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global
trait Pool[T] {
  def get:T
  def release(using r : T) : Unit
  def toAll(f : T => Unit) : Unit
  //def clear: Unit
  
}

object Pool:
  case class State(in : Int, out : Int)
  inline def fromPool[T](using  T) : T = summon
  def apply[T](max : Int,pv : ()=>T) : Pool[T] =
    PoolImpl[T](pv,max)
  case class Res[T](var datas : List[T])
  class PoolImpl[T](pvd : () => T,
                    max : Int,
                    var out : Int = 0,val data : Res[T]=Res[T](Nil),val all : Res[T]=Res[T](Nil)) extends Pool[T]:

    def state : State = State(data.datas.size : Int, out)
    inline def printSate = println(s"pool : ${state}")
    def get : T =

      data.synchronized{
        if !data.datas.isEmpty
        then
          val res = data.datas.head
          data.datas = data.datas.tail
          out+=1
          res
        else
          if data.datas.size + out < max then
            if data.datas.size < max then
              val n = pvd()
              all.datas = n  +: all.datas
              data.datas = n  +: data.datas
            val res = data.datas.head
            data.datas = data.datas.tail
            out+=1
            res
          else
            data.wait()
            get
        }

    def toAll(f : T => Unit) =
      all.datas.foreach(f)
    def release(using r : T) : Unit =
      data.synchronized{
      data.datas = data.datas :+ r
      out-=1

      data.notify()
      }

@main def testPool() =
  import Pool.fromPool
  val pool = Pool(3,() => "test")

  Future{
    given String = pool.get

    println(fromPool)
    Thread.sleep(2000)
    pool.release

  }
  Future{
    given String = pool.get

    println(fromPool)
    Thread.sleep(4000)
    pool.release
  }
  Future{
    given String = pool.get

    println(fromPool)
    Thread.sleep(2000)
    pool.release
  }
  Future{
    given String = pool.get

    println(fromPool)
    Thread.sleep(2000)
    pool.release
  }
  Future{
    given String = pool.get

    println(fromPool)
    Thread.sleep(2000)
    pool.release
  }
  Future{
    given String = pool.get

    println(fromPool)
    Thread.sleep(2000)
    pool.release
  }

  Thread.sleep(500000)



