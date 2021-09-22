package bon.jo.datamodeler.server

import akka.actor.typed.ActorRef
import bon.jo.datamodeler.model.Model.User
import bon.jo.datamodeler.server.Response
// Trait and its implementations representing all possible messages that can be sent to this Behavior
enum Command[T]:
  case Add(user: T, replyTo: ActorRef[T])
  case GetById(id: Int, replyTo: ActorRef[Option[T]])
  case Clear(replyTo: ActorRef[Response])
