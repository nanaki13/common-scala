package bon.jo.datamodeler.server

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

import bon.jo.datamodeler.model.sql.DaoOps
import bon.jo.datamodeler.server.Response
import bon.jo.datamodeler.server.Response.*
import bon.jo.datamodeler.server.Command
import bon.jo.datamodeler.server.Command.*
object Repository {
  // This behavior handles all possible incoming messages and keeps the state in the function parameter
  def apply[T](dao: DaoOps.Sync[T, Int]): Behavior[Command[T]] = Behaviors.receiveMessage {
    case Add(job, replyTo) =>
      replyTo ! dao.save(job)
      Behaviors.same
    case GetById(id, replyTo) =>
      println(dao.selectById(id))
      replyTo ! dao.selectById(id)
      Behaviors.same
    case Clear(replyTo) =>
      dao.deleteAll()
      replyTo ! OK
      Behaviors.same
    case DeleteById(id, replyTo) =>
      val resp = if dao.deleteById(id) == 0 then KO("not found") else OK
      replyTo ! resp
      Behaviors.same
    case UpdateById(id,user, replyTo) =>
      println(id)
      val userUpdate = if dao.update(id,user) == 0 then KO("not found") else OK
      replyTo ! userUpdate 
      Behaviors.same
    case GetAll(page,replyTo) =>
      replyTo ! dao.selectAll(page)
      Behaviors.same  

  }
}
