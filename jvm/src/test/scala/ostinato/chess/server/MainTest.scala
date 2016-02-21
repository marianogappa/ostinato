package ostinato.chess.server

import akka.event.Logging
import akka.http.scaladsl.server.Route
import org.scalatest.{FunSpec, Matchers}
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.testkit.ScalatestRouteTest

class MainTest extends FunSpec with Matchers with ScalatestRouteTest with OstinatoServerRoute {
  val logger = Logging(system, getClass)

  describe("Ostinato Server") {
    it("should respond OK! when healthchecked") {
      Get("/healthcheck") ~> Route.seal(route) ~> check {
        status shouldEqual StatusCodes.OK
        responseAs[String] shouldEqual "OK!"
      }
    }
  }
}
