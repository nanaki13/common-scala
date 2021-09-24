package bon.jo.datamodeler.server

import akka.actor.typed.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.HttpMethods.{DELETE, GET, POST, PUT}
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpMethod, HttpRequest, StatusCodes}
import akka.http.scaladsl.unmarshalling.Unmarshal
import bon.jo.datamodeler.model.Model.User
import bon.jo.datamodeler.model.{Server, UserJsonSupport}
import org.scalatest.*
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport.*
import bon.jo.datamodeler.model.sql.SimpleSql
import bon.jo.datamodeler.util.{ConnectionPool, Utils}
import spray.json.DefaultJsonProtocol.*
import spray.json.RootJsonFormat
import bon.jo.datamodeler.util.Pool

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration.*
import scala.concurrent.duration.*
class TestServer extends AnyFlatSpec with should.Matchers with SprayJsonSupport:
  private val host = "localhost"
  val port = 8080
  def url = s"http://$host:$port/user"
  val server = Server.test(host, port)
  given system: ActorSystem[Server.Message] =  ActorSystem(server(), "BuildJobsServer")

  given ExecutionContext = system.executionContext
  val s : UserJsonSupport  = UserJsonSupport()
  import s.given

  given Pool[java.sql.Connection] = server.pool
  ConnectionPool.onStmtDo{
    SimpleSql.dropTable[User]
    SimpleSql.createTable[User]
  }

  extension[A](a : A)(using RootJsonFormat[A])
    def jsString() : String = summon[RootJsonFormat[A]].write(a).compactPrint

  def req[A](m : HttpMethod)(a : A)(using RootJsonFormat[A]) : HttpRequest =
    HttpRequest(m,url,
      entity = HttpEntity.apply(contentType = ContentTypes.`application/json`,
        a.jsString()) )
  def put[A](a : A)(using RootJsonFormat[A]) : HttpRequest =
    req(PUT)(a)
  def post[A](a : A)(using RootJsonFormat[A]) : HttpRequest =
    req(POST)(a)
  def wait[A](f : Future[A],second : Int = 5) = Await.result(f,second.second)

  val userRef = User(name = "John")

  "A server" can " deal with post user" in {

    val r = post(userRef)
    val respF : Future[User] = Http().singleRequest(r).flatMap{
      resp =>
        resp.status should be(StatusCodes.OK)
        Unmarshal (resp).to[User]

    }
    val rest = wait(respF)
    rest.name should be ("John")

  }

  "A server" can " deal with get user" in {

    val r = HttpRequest(GET,url+"/1")
    val respF : Future[User] = Http().singleRequest(r).flatMap {
      resp =>
        resp.status should be(StatusCodes.OK)
        Unmarshal(resp).to[User]

    }
    val rest = wait(respF)
    rest.name should be ("John")

  }
  "A server" can " deal with put user an delete" in {

    var r = HttpRequest(PUT,url+"/1",
      entity = HttpEntity.apply(contentType = ContentTypes.`application/json`,
        userRef.copy(id= 22,name = "Boasb").jsString()) )
    val rest = Http().singleRequest(r).map{ resp =>
      resp.status
    }
    wait(rest) should be (StatusCodes.NoContent)
    r = HttpRequest(DELETE,url+"/22")
    val respStatus = Http().singleRequest(r).map(_.status)
    val stat = wait(respStatus)
    stat should be (StatusCodes.NoContent)
    system.terminate()
  }
  /*"A server" can " deal with delete user" in {

    val r = HttpRequest(DELETE,url+"/1")
    val respStatus = Http().singleRequest(r).map(_.status)
    val rest = wait(respStatus)
    rest should be (StatusCodes.NoContent)
    system.terminate()

  }*/




