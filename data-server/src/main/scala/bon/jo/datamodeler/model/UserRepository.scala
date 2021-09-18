package bon.jo.datamodeler.model

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import bon.jo.datamodeler.model.Model.{User, UserRoom}
import bon.jo.datamodeler.model.sql.{DaoOps, RawDao, RawDaoOps}
import bon.jo.datamodeler.util.{ConnectionPool, Pool}

object UserRepository {


  // Trait defining successful and failure responses
  enum Response:
    case  OK
    case  OKSave(r : User)
    case  KO(reason: String)

  // Trait and its implementations representing all possible messages that can be sent to this Behavior
  enum Command:
     case  AddUser(user: User, replyTo: ActorRef[Response])
     case  GetUserById(id: Int, replyTo: ActorRef[Option[User]])
     case  ClearUsers(replyTo: ActorRef[Response])
  import Command.*
  import Response.*

  // This behavior handles all possible incoming messages and keeps the state in the function parameter
  def apply(dao : DaoOps.Sync[User,Int]): Behavior[Command] = Behaviors.receiveMessage {
    case AddUser(job, replyTo) =>
      replyTo ! OKSave( dao.save(job))
      Behaviors.same
    case GetUserById(id, replyTo) =>
      println(dao.selectById(id))
      replyTo ! dao.selectById(id)
      Behaviors.same
    case ClearUsers(replyTo) =>
      dao.deleteAll()
      replyTo ! OK
      Behaviors.same
  }
}
@main def te() =
  import UserRepository.*
  import Command.*
  import Response.*
  given Pool[java.sql.Connection] = ConnectionPool(10)("jdbc:sqlite:sample2.db","org.sqlite.JDBC")


  def reponse() : Behavior[Response]=
    Behaviors.setup { context =>
      Behaviors.receiveMessage { message =>
        println(message)
        Behaviors.same
      }
    }


  val userDao : DaoOps.Sync[User,Int] = DaoOps.Sync.fromPool((id,e)=>e.copy(id = id))

  def m() :  Behavior[Command] =
    Behaviors.setup { context =>
      println("setup sys")
      val userRepositoryActor = context.spawn(UserRepository(userDao), "UserRepository")

      Behaviors.receiveMessage { message =>
        println(s"receive $message")
        val provresss =  context.spawn( reponse(), "resposProcess")
        message match {
          case m @ AddUser(use,_) =>
            userRepositoryActor !  m.copy(replyTo = provresss)
          case _ => println(s"not handle : $message")
        }

        Behaviors.same
      }
    }


  val system: ActorSystem[Command] = ActorSystem(m(),"root")


  system ! AddUser(User(name = "test"),null)
