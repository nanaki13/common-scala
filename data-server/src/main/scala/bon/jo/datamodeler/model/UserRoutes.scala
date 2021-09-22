package bon.jo.datamodeler.model

import akka.actor.typed.{ActorRef, ActorSystem}
import akka.util.Timeout
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import bon.jo.datamodeler.model.Model.User
import bon.jo.datamodeler.server.{Command, Repository, Response}
import Command.*
import Response.*
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport

import scala.concurrent.duration.*
import scala.concurrent.Future

class UserRoutes[T](baseRoute : String,buildJobRepository: ActorRef[Command[T]],support : JsonSupport[T])(using ActorSystem[_]) extends SprayJsonSupport {

  import akka.actor.typed.scaladsl.AskPattern.schedulerFromActorSystem
  import akka.actor.typed.scaladsl.AskPattern.Askable
  //val support : JsonSupport[T] = summon[JsonSupport[T]]
  //import spray.json.DefaultJsonProtocol._
  import support.given

  // asking someone requires a timeout and a scheduler, if the timeout hits without response
  // the ask is failed with a TimeoutException
  implicit val timeout: Timeout = 3.seconds


  lazy val theJobRoutes: Route =
    pathPrefix(baseRoute) {
      concat(
        pathEnd {
          concat(
            post {
              entity(as[T]) { job =>
                val operationPerformed: Future[T] = buildJobRepository.ask(Add(job, _))
                complete(operationPerformed)
              }
            },
            delete {
              val operationPerformed: Future[Response] =
                buildJobRepository.ask(Clear(_))
              onSuccess(operationPerformed) {
                case OK         => complete("Jobs cleared")
                case KO(reason) => complete(StatusCodes.InternalServerError -> reason)

              }
            }
          )
        },
        (get & path(IntNumber)) { id =>
          val maybeJob: Future[Option[T]] =
            buildJobRepository.ask(GetById(id, _))
          rejectEmptyResponse {
            complete(maybeJob)
          }
        }
      )
    }
}