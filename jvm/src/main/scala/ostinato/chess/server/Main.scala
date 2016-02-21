package ostinato.chess.server

import akka.actor.ActorSystem
import akka.event.{LoggingAdapter, Logging}
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory

object Main extends App with OstinatoServerRoute {
  implicit val system = ActorSystem("ostinato")
  implicit val materializer = ActorMaterializer()
  implicit val ec = system.dispatcher

  val config = ConfigFactory.load()
  val logger = Logging(system, getClass)


  val bindingFuture = Http().bindAndHandle(route, "localhost", 51234)
}

trait OstinatoServerRoute {
  val logger: LoggingAdapter

  val route =
    path("healthcheck") {
      get {
        complete {
          logger.debug("Healtcheck requested")
          "OK!"
        }
      }
    }
}
