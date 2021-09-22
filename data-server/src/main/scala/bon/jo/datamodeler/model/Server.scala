package bon.jo.datamodeler.model

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, PostStop}
import akka.http.scaladsl.Http.ServerBinding
import akka.http.scaladsl.Http
import bon.jo.datamodeler.model.Model.User
import bon.jo.datamodeler.model.sql.DaoOps
import bon.jo.datamodeler.server.{Command, Repository}
import bon.jo.datamodeler.util.{ConnectionPool, Pool}

import scala.concurrent.Future
import scala.util.{Failure, Success}
import Server.Message
import Server.Message.*

object Server :
  enum Message :
    private [Server] case StartFailed(cause: Throwable)
    private [Server] case Started(binding: ServerBinding)
    private [Server] case Stop
  class ServerImpl( val host: String,
  val port: Int)(using pool : Pool[java.sql.Connection])extends Server
  def test( host: String,
   port: Int):Server =
    given Pool[java.sql.Connection] = ConnectionPool(10)("jdbc:sqlite:sample2.db","org.sqlite.JDBC")
    ServerImpl( host,port)
trait Server(using Pool[java.sql.Connection]) {

  val host: String
  val port: Int
  def apply(): Behavior[Message] = Behaviors.setup { ctx =>

    implicit val system = ctx.system

    val userDao : DaoOps.Sync[User,Int] = DaoOps.Sync.fromPool((id,e)=>e.copy(id = id))
    userDao.save(User(name = "test"))
    userDao.save(User(name = "toto"))
    given ActorRef[Command[User]] = ctx.spawn(Repository(userDao), "JobRepository")
    given JsonSupport[User] = UserJsonSupport()
    val routes = new UserRoutes("user")

    val serverBinding: Future[Http.ServerBinding] =
      Http().newServerAt(host, port).bind(routes.theJobRoutes)
    ctx.pipeToSelf(serverBinding) {
      case Success(binding) => Started(binding)
      case Failure(ex)      => StartFailed(ex)
    }

    def running(binding: ServerBinding): Behavior[Message] =
      Behaviors.receiveMessagePartial[Message] {
        case Stop =>
          ctx.log.info(
            "Stopping server http://{}:{}/",
            binding.localAddress.getHostString,
            binding.localAddress.getPort)
          Behaviors.stopped
      }.receiveSignal {
        case (_, PostStop) =>
          binding.unbind()
          Behaviors.same
      }

    def starting(wasStopped: Boolean): Behaviors.Receive[Message] =
      Behaviors.receiveMessage[Message] {
        case StartFailed(cause) =>
          throw new RuntimeException("Server failed to start", cause)
        case Started(binding) =>
          ctx.log.info(
            "Server online at http://{}:{}/",
            binding.localAddress.getHostString,
            binding.localAddress.getPort)
          if (wasStopped) ctx.self ! Stop
          running(binding)
        case Stop =>
          // we got a stop message but haven't completed starting yet,
          // we cannot stop until starting has completed
          starting(wasStopped = true)
      }

    starting(wasStopped = false)
  }
}

@main def serverTest(): Unit = {
  val system: ActorSystem[Server.Message] =
    ActorSystem(Server.test("localhost", 8080)(), "BuildJobsServer")
  //bon.jo.datamodeler.model.te()
}