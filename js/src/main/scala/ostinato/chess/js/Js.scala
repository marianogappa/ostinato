package ostinato.chess.js

import ostinato.chess.api.Api
import ostinato.chess.core._
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel(name="Js")
object Js {
  val defaultGame: String = ChessGame.defaultGame.toFen
  val api = new Api

  def move(ostinatoString: String, from: String, to: String) =
    sjs(api.move(ostinatoString, from, to))

  def randomAiMove(fen: String) = sjs(api.randomAiMove(fen))

  def parseNotation(input: String) = sjs(api.parseNotation(input))

  def convertNotation(input: String, notation: String) =
    sjs(api.convertNotation(input, notation))

  def basicAiMove(fen: String, _depth: Int, _debug: Boolean) =
    sjs(api.basicAiMove(fen, _depth, _debug))

  def initialBoard() = sjs(Map("board" -> defaultGame))

  private def sjs(m: Map[String, Any]) =
    m.mapValues {
      case a: Array[_] ⇒ a.toJSArray
      case e ⇒ e
    }.toJSDictionary
}
