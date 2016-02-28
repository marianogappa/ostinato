package ostinato.chess.js

import ostinato.chess.ai.{ChessRandomAi, ChessBasicAi}
import ostinato.chess.core.NotationParser.{ SuccessfulParse, FailedParse, ParsedMatch }
import ostinato.chess.core._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExportAll, JSExport}
import js.JSConverters._

@JSExport @JSExportAll
object Js {
  val defaultGame: String = ChessGame.defaultGame.toFen

  def isFinalBoard(fen: String): Boolean = ChessGame.fromFen(fen).get.isGameOver

  def move(fen: String, from: String, to: String): String = {
    val fromPos = ChessXY.fromAn(from).get
    val toPos = ChessXY.fromAn(to).get
    val board = ChessGame.fromFen(fen).get.board
    val action = board.movementsOfDelta(fromPos, toPos - fromPos).headOption

    if (action.isEmpty)
      ""
    else
      board.doAction(action.get).get.toFen
  }

  def basicAiMove(fen: String, _player: String, _depth: Int, _debug: Boolean): String = {
    val player = if (Set("white", "w") contains _player.toLowerCase) WhiteChessPlayer else BlackChessPlayer
    val game = ChessGame.fromFen(fen).get
    val action = ChessBasicAi(player, debug = _debug, depth = _depth).nextAction(game).get
    game.board.doAction(action).get.toFen
  }

  def randomAiMove(fen: String, _player: String): String = {
    val player = if (Set("white", "w") contains _player.toLowerCase) WhiteChessPlayer else BlackChessPlayer
    val game = ChessGame.fromFen(fen).get
    val action = ChessRandomAi(player).nextAction(game).get
    game.board.doAction(action).get.toFen
  }

  def parseNotation(input: String): js.Dictionary[Any] = {
    val results = NotationParser.parseMatchString(input).results

    results.head match {
      case parsedMatch @ ParsedMatch(steps, notationRules) ⇒
        val fenBoards = steps.filter(_.maybeGameStep.nonEmpty).map(_.maybeGameStep.get.board.toFen).toArray

        val actions = parsedMatch.actionStrings

        val validActionCount = parsedMatch.validStepCount

        val parseWasSuccessful = notationRules match {
          case SuccessfulParse(_) ⇒ true
          case FailedParse(_)     ⇒ false
        }

        val notationName = notationRules match {
          case SuccessfulParse(r: NotationRules)   ⇒ r.fullName
          case FailedParse(Some(r: NotationRules)) ⇒ r.fullName
          case FailedParse(None)                   ⇒ ""
        }

        Map(
          "fenBoards" -> fenBoards.toJSArray,
          "actions" -> actions.toJSArray,
          "validActionCount" -> validActionCount,
          "parseWasSuccessful" -> parseWasSuccessful,
          "notationName" -> notationName
        ).toJSDictionary
    }
  }
}
