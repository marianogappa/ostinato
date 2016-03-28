package ostinato.chess.js

import ostinato.chess.api.Api
import ostinato.chess.core._

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.{JSExport, JSExportAll}

@JSExport @JSExportAll
object Js {
  val defaultGame: String = ChessGame.defaultGame.toFen

  def move(ostinatoString: String, from: String, to: String) = sjs(Api.move(ostinatoString, from, to))
  def randomAiMove(fen: String, _player: String) = sjs(Api.randomAiMove(fen, _player))
  def parseNotation(input: String) = sjs(Api.parseNotation(input))
  def convertNotation(input: String, notation: String) = sjs(Api.convertNotation(input, notation))

  def basicAiMove(fen: String, _player: String, _depth: Int, _debug: Boolean) =
    sjs(Api.basicAiMove(fen, _player, _depth, _debug))

  private def sjs(m: Map[String, Any]) = m.mapValues {
    case a: Array[_] ⇒ a.toJSArray
    case e           ⇒ e
  }.toJSDictionary
}
