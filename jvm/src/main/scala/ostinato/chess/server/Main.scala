package ostinato.chess.server

import akka.actor.ActorSystem
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.StandardRoute
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import ostinato.chess.api.Api
import java.net.URLDecoder
import spray.json.DefaultJsonProtocol
import spray.json._

import ostinato.chess.core.ChessPlayer

object Main extends App with OstinatoServerRoute {
  implicit val system = ActorSystem("ostinato")
  implicit val materializer = ActorMaterializer()
  implicit val ec = system.dispatcher

  val config = ConfigFactory.load()
  val logger = Logging(system, getClass)

  val bindingFuture = Http().bindAndHandle(route, "0.0.0.0", 51234)
  bindingFuture foreach (_ =>
    println(
      "Ostinato server is listening on port 51234. Try `curl localhost:51234/healthcheck`."))
}

case class RequestMove(board: String, from: String, to: String)

case class RequestBasicAI(board: String, depth: Option[Int], debug: Option[Boolean])

case class RequestRandomAI(board: String)

case class RequestParseNotation(input: String)

case class RequestConvertNotation(input: String, notation: String)

trait JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
  implicit val f1 = jsonFormat3(RequestMove.apply)
  implicit val f2 = jsonFormat3(RequestBasicAI.apply)
  implicit val f3 = jsonFormat1(RequestRandomAI.apply)
  implicit val f4 = jsonFormat1(RequestParseNotation.apply)
  implicit val f5 = jsonFormat2(RequestConvertNotation.apply)
}

trait OstinatoServerRoute extends JsonSupport {

  implicit object AnyJsonFormat extends JsonFormat[Any] {
    def write(x: Any) = x match {
      case n: Int => JsNumber(n)
      case s: String => JsString(s)
      case b: Boolean if b == true => JsTrue
      case b: Boolean if b == false => JsFalse
    }

    def read(value: JsValue) = value match {
      case JsNumber(n) => n.intValue()
      case JsString(s) => s
      case JsTrue => true
      case JsFalse => false
      case _ => "unimplemented"
    }
  }

  val logger: LoggingAdapter

  val optionsHeaders = List(
    RawHeader("Access-Control-Allow-Origin", "*"),
    RawHeader("Access-Control-Allow-Methods",
              "GET, POST, PUT, OPTIONS, DELETE"),
    RawHeader("Access-Control-Allow-Headers",
              "Origin, X-Requested-With, Content-Type, Accept, Authorization")
  )

  val api = ServerApi()

  val route =
    path("healthcheck") {
      get {
        complete {
          logger.debug("Healtcheck requested")
          "OK!"
        }
      }
    } ~
      post {
        respondWithAllowOrigin {
          path("move") {
            entity(as[RequestMove]) { r =>
              complete {
                api.move(r.board, r.from, r.to)
              }
            }
          } ~
            path("basicAiMove") {
              entity(as[RequestBasicAI]) { r =>
                complete {
                  api.basicAiMove(r.board, r.depth.getOrElse(2), r.debug.getOrElse(false))
                }
              }
            } ~
            path("randomAiMove") {
              entity(as[RequestRandomAI]) { r =>
                complete {
                  api.randomAiMove(r.board)
                }
              }
            } ~
            path("parseNotation") {
              entity(as[RequestParseNotation]) { r =>
                complete {
                  api.parseNotation(r.input)
                }
              }
            } ~
            path("convertNotation") {
              entity(as[RequestConvertNotation]) { r =>
                complete {
                  api.convertNotation(r.input, r.notation)
                }
              }
            }
        }
      } ~
      path("initialBoard") {
        complete {
          Map("board" -> api.defaultGame)
        }
      } ~ options {
      complete {
        HttpResponse().withHeaders(optionsHeaders)
      }
    }

  def respondWithAllowOrigin =
    respondWithHeader(RawHeader("Access-Control-Allow-Origin", "*"))

}

case class ServerApi() extends Api {
  override protected def instantiateChessBasicAi(_player: ChessPlayer,
                                                 _depth: Int,
                                                 _debug: Boolean) =
    new ParallelisedChessBasicAi(_player, _depth, _debug)
}
