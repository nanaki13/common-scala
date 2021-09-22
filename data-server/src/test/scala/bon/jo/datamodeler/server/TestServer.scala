package bon.jo.datamodeler.server

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.HttpMethods.{GET, POST}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpRequest}
import akka.http.scaladsl.unmarshalling.Unmarshal
import bon.jo.datamodeler.model.Model.User
import bon.jo.datamodeler.model.{Server, UserJsonSupport}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import spray.json.DefaultJsonProtocol.*

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
class TestServer extends AnyFlatSpec with should.Matchers with SprayJsonSupport:
  given system: ActorSystem[Server.Message] = ActorSystem(Server.test("localhost", 8080)(), "BuildJobsServer")
  given ExecutionContext = system.executionContext
  val s : UserJsonSupport  = UserJsonSupport()
  import s.given
  "A server" can " handele post user" in {

    val r = HttpRequest(POST,"http://localhost:8080/user",entity = HttpEntity.apply(contentType = ContentTypes.`application/json`,  s.format.write(User(0,"sdf")).compactPrint) )
    val resp = Http().singleRequest(r)
    resp.flatMap(Unmarshal(_).to[User])
    val pet: Future[User] = resp.flatMap(Unmarshal(_).to[User])
    println( Await.result(pet,Duration.Inf))

  }

  "A server" can " handele get user" in {

    val r = HttpRequest(GET,"http://localhost:8080/user/1" )
    val resp = Http().singleRequest(r)
    resp.flatMap(Unmarshal(_).to[User])
    val pet: Future[User] = resp.flatMap(Unmarshal(_).to[User])
    println( Await.result(pet,Duration.Inf))

  }



