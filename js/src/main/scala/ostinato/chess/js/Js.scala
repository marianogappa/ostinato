package ostinato.chess.js

import ostinato.chess.ai.{ ChessRandomAi, ChessBasicAi }
import ostinato.chess.core.NotationParser.{ SuccessfulParse, FailedParse, ParsedMatch }
import ostinato.chess.core._

import scala.scalajs.js
import scala.scalajs.js.annotation.{ JSExportAll, JSExport }
import js.JSConverters._

@JSExport @JSExportAll
object Js {
  val defaultGame: String = ChessGame.defaultGame.toFen

  def isFinalBoard(fen: String): Boolean = ChessGame.fromOstinatoString(fen).get.isGameOver

  def move(ostinatoString: String, from: String, to: String): js.Dictionary[Any] = {
    val fromPos = ChessXY.fromAn(from).get
    val toPos = ChessXY.fromAn(to).get
    val game = ChessGame.fromOstinatoString(ostinatoString).toOption
    val action = game flatMap (_.board.movementsOfDelta(fromPos, toPos - fromPos).headOption)

    moveResult(action, game)
  }

  def basicAiMove(fen: String, _player: String, _depth: Int, _debug: Boolean): js.Dictionary[Any] = {
    val player = if (Set("white", "w") contains _player.toLowerCase) WhiteChessPlayer else BlackChessPlayer
    val game = ChessGame.fromOstinatoString(fen).toOption
    val action = game flatMap (ChessBasicAi(player, debug = _debug, depth = _depth).nextAction(_))

    moveResult(action, game)
  }

  def randomAiMove(fen: String, _player: String): js.Dictionary[Any] = {
    val player = if (Set("white", "w") contains _player.toLowerCase) WhiteChessPlayer else BlackChessPlayer
    val game = ChessGame.fromOstinatoString(fen).toOption
    val action = game flatMap (ChessRandomAi(player).nextNonFinalAction(_))

    moveResult(action, game)
  }

  private def moveResult(action: Option[ChessAction], game: Option[ChessGame]): js.Dictionary[Any] = {
    (for {
      a ← action
      g ← game
      b = g.board
      nb ← b.doAction(a)
    } yield {
      Map(
        "success" -> true,
        "board" -> nb.toOstinatoString,
        "action" -> a.toAn,
        "isCheck" -> a.isCheck,
        "isCheckmate" -> a.isCheckmate,
        "isDraw" -> (!a.isCheckmate && a.isFinal)
      ).toJSDictionary
    }) getOrElse Map("success" -> (false: Any)).toJSDictionary
  }

  def parseNotation(input: String): js.Dictionary[Any] = {
    val results = NotationParser.parseMatchString(input).results

    results.head match {
      case parsedMatch @ ParsedMatch(steps, notationRules) ⇒
        val boards = steps.filter(_.maybeGameStep.nonEmpty).map(_.maybeGameStep.get.board.toOstinatoString).toArray

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
          "boards" -> boards.toJSArray,
          "actions" -> actions.toJSArray,
          "validActionCount" -> validActionCount,
          "parseWasSuccessful" -> parseWasSuccessful,
          "notationName" -> notationName
        ).toJSDictionary
    }
  }

  def convertNotation(input: String, notation: String): js.Dictionary[Any] = {
    val results = NotationParser.parseMatchString(input)

    Map(
      "actions" ->
        results.parsedMatches.head.flatMap(
          _.maybeGameStep.map(
            gameStep ⇒ getActionParser(notation).serialiseAction(gameStep.action).head._1
          )
        ).toJSArray,
      "validActionCount" -> results.results.head.validStepCount
    ).toJSDictionary
  }

  private def getActionParser(notation: String) = notation match {
    case "Algebraic Notation" ⇒
      AlgebraicNotationActionSerialiser(
        AlgebraicNotationRules(
          lowerCaseLetters = true,
          figurine = false,
          distinguishCaptures = true,
          colonForCaptures = false,
          castlingNotation = "zeroes"
        )
      )
    case "Figurine Algebraic Notation" ⇒
      AlgebraicNotationActionSerialiser(AlgebraicNotation.allPossibleRules.head.copy(figurine = true))
    case "Descriptive Notation" ⇒
      DescriptiveNotationActionSerialiser(DescriptiveNotation.allPossibleRules.head)
    case "Coordinate Notation" ⇒
      CoordinateNotationActionSerialiser(CoordinateNotation.allPossibleRules.head)
    case "ICCF Notation" ⇒
      IccfNotationActionSerialiser(IccfNotation.allPossibleRules.head)
    case "Smith Notation" ⇒
      SmithNotationActionSerialiser(SmithNotation.allPossibleRules.head)
    case _ ⇒ AlgebraicNotationActionSerialiser(
      AlgebraicNotationRules(
        lowerCaseLetters = true,
        figurine = false,
        distinguishCaptures = true,
        colonForCaptures = false,
        castlingNotation = "zeroes"
      )
    )
  }
}
