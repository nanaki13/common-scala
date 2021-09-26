package bon.jo.datamodeler.server

import akka.actor.typed.ActorRef
import bon.jo.datamodeler.model.Model.User
import bon.jo.datamodeler.model.Page
import bon.jo.datamodeler.server.Response
// Trait and its implementations representing all possible messages that can be sent to this Behavior
enum Command[T]:
  case Add(user: T, replyTo: ActorRef[T])
  case GetById(id: Int, replyTo: ActorRef[Option[T]])
  case Clear(replyTo: ActorRef[Response])
  case DeleteById(id: Int, replyTo: ActorRef[Response])
  case UpdateById(id: Int,user: T, replyTo:ActorRef[Response])
  case GetAll(page: Page.Request,replyTo : ActorRef[Page.Response[T]] )
