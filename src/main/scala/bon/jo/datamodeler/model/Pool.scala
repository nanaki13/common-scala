package bon.jo.datamodeler.model

import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global
trait Pool[T] {
  def get:T
  def release(r : T) : Unit
}

object Pool:
  def apply[T](max : Int,pv : ()=>T) : Pool[T] =
    PoolImpl[T](pv,max)
  case class Res[T](var datas : List[T])
  class PoolImpl[T](pvd : () => T,
                    max : Int,
                    var out : Int = 0,data : Res[T]=Res[T](Nil)) extends Pool[T]:
    def get : T =

       data.synchronized{
          if data.datas.size + out < max then
            if data.datas.size < max then
              data.datas = pvd() +: data.datas
            val res = data.datas.head
            data.datas = data.datas.tail
            out+=1
            res
          else if !data.datas.isEmpty then
            val res = data.datas.head
            data.datas = data.datas.tail
            out+=1
            res
          else
            data.wait()
            get
        }


    def release(r : T) : Unit =
      data.synchronized{
      data.datas = data.datas :+ r
      out-=1
      data.notify()
      }

@main def testPool() =
  val pool = Pool(3,() => "test")

  Future{
    val e = pool.get

    println(e)
    Thread.sleep(2000)
    pool.release("test")

  }
  Future{
    val e = pool.get

    println(e)
    Thread.sleep(4000)
    pool.release("test")
  }
  Future{
    val e = pool.get

    println(e)
    Thread.sleep(2000)
    pool.release("test")
  }
  Future{
    val e = pool.get

    println(e)
    Thread.sleep(2000)
    pool.release("test")
  }
  Future{
    val e = pool.get

    println(e)
    Thread.sleep(2000)
    pool.release("test")
  }
  Future{
    val e = pool.get

    println(e)
    Thread.sleep(2000)
    pool.release("test")
  }

  Thread.sleep(500000)



