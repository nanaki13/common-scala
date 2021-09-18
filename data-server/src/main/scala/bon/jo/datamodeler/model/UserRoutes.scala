package bon.jo.datamodeler.model

import akka.actor.typed.{ActorRef, ActorSystem}
import akka.util.Timeout
import akka.http.scaladsl.server.Directives.*
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import bon.jo.datamodeler.model.Model.User

import scala.concurrent.duration.*
import scala.concurrent.Future

class UserRoutes(buildJobRepository: ActorRef[UserRepository.Command])(implicit system: ActorSystem[_]) extends JsonSupport {

  import akka.actor.typed.scaladsl.AskPattern.schedulerFromActorSystem
  import akka.actor.typed.scaladsl.AskPattern.Askable

  // asking someone requires a timeout and a scheduler, if the timeout hits without response
  // the ask is failed with a TimeoutException
  implicit val timeout: Timeout = 3.seconds

  import UserRepository.Command.*
  import UserRepository.Response.*
  lazy val theJobRoutes: Route =
    pathPrefix("users") {
      concat(
        pathEnd {
          concat(
            post {
              entity(as[User]) { job =>
                val operationPerformed: Future[UserRepository.Response] =
                  buildJobRepository.ask(AddUser(job, _))
                onSuccess(operationPerformed) {

                  case KO(reason) => complete(StatusCodes.InternalServerError -> reason)
                  case OKSave(s) => complete(s)
                  case _         => complete(StatusCodes.InternalServerError -> "not gandle response")
                }
              }
            },
            delete {
              val operationPerformed: Future[UserRepository.Response] =
                buildJobRepository.ask(ClearUsers(_))
              onSuccess(operationPerformed) {
                case OK         => complete("Jobs cleared")
                case KO(reason) => complete(StatusCodes.InternalServerError -> reason)
              }
            }
          )
        },
        (get & path(IntNumber)) { id =>
          val maybeJob: Future[Option[User]] =
            buildJobRepository.ask(GetUserById(id, _))
          rejectEmptyResponse {
            complete(maybeJob)
          }
        }
      )
    }
}