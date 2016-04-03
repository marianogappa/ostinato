package ostinato.chess.server

import akka.actor.ActorSystem
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.model.headers._
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.StandardRoute
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import ostinato.chess.api.Api
import java.net.URLDecoder

import ostinato.chess.core.ChessPlayer

object Main extends App with OstinatoServerRoute {
  implicit val system = ActorSystem("ostinato")
  implicit val materializer = ActorMaterializer()
  implicit val ec = system.dispatcher

  val config = ConfigFactory.load()
  val logger = Logging(system, getClass)

  val bindingFuture = Http().bindAndHandle(route, "0.0.0.0", 51234)
}

trait OstinatoServerRoute {
  val logger: LoggingAdapter

  val optionsHeaders = List(RawHeader("Access-Control-Allow-Origin", "*"),
    RawHeader("Access-Control-Allow-Methods", "GET, POST, PUT, OPTIONS, DELETE"),
    RawHeader("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept, Authorization"))

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
          path("move") {
            entity(as[String])(serve[Map[String, (String, String, String)]](_, {
              case m => (api.move _).tupled(m("data")) }))
          } ~
            path("basicAiMove") {
              entity(as[String])(serve[Map[String, (String, String, Int, Boolean)]](_, {
                case m => (api.basicAiMove _).tupled(m("data"))}))
            } ~
            path("randomAiMove") {
              entity(as[String])(serve[Map[String, (String, String)]](_, {
                case m => (api.randomAiMove _).tupled(m("data"))}))
            } ~
            path("parseNotation") {
              entity(as[String])(serve[Map[String, (String, Boolean)]](_, {
                case m => (api.parseNotation _).tupled(m("data"))}))
            } ~
            path("convertNotation") {
              entity(as[String])(serve[Map[String, (String, String)]](_, {
                case m => (api.convertNotation _).tupled(m("data"))}))
            }
        } ~ options {
      complete { HttpResponse().withHeaders(optionsHeaders) }
    }

  private def serve[T: Manifest](request: String, f: T => Map[String, Any]): StandardRoute = {
    complete (
      HttpResponse(
        entity =
        JsonUtil.toJson (
          f (
            JsonUtil.fromJson[T](URLDecoder.decode(request, "UTF-8"))
          )
        )
      ).withHeaders(`Access-Control-Allow-Origin`.`*`)
    )
  }
}

case class ServerApi() extends Api {
  override protected def instantiateChessBasicAi(_player: ChessPlayer, _depth: Int, _debug: Boolean) =
    new ParallelisedChessBasicAi(_player, _depth, _debug)
}
