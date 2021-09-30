package bon.jo.datamodeler.server

import akka.actor.typed.ActorRef
import bon.jo.datamodeler.model.Model.User
import bon.jo.datamodeler.model.Page
import bon.jo.datamodeler.model.sql.Filtre
import bon.jo.datamodeler.server.Response

enum Command[T]:
  case Add(user: T, replyTo: ActorRef[T])
  case GetById(id: Int, replyTo: ActorRef[Option[T]])
  case Clear(replyTo: ActorRef[Response])
  case DeleteById(id: Int, replyTo: ActorRef[Response])
  case UpdateById(id: Int,user: T, replyTo:ActorRef[Response])
  case GetAll(page: Page.Request, filtre: Filtre, replyTo : ActorRef[Page.Response[T]] )
