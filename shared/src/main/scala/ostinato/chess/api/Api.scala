package ostinato.chess.api

import ostinato.chess.ai.{ChessBasicAi, ChessRandomAi}
import ostinato.chess.core.NotationParser.{FailedParse, ParsedMatch, SuccessfulParse}
import ostinato.chess.core.{AlgebraicNotation, AlgebraicNotationActionSerialiser, AlgebraicNotationRules, BlackChessPlayer, ChessAction, ChessGame, ChessPlayer, ChessXY, CoordinateNotation, CoordinateNotationActionSerialiser, DescriptiveNotation, DescriptiveNotationActionSerialiser, IccfNotation, IccfNotationActionSerialiser, NotationParser, NotationRules, SmithNotation, SmithNotationActionSerialiser, WhiteChessPlayer}

class Api {
  val defaultGame: String = ChessGame.defaultGame.toFen

  def move(ostinatoString: String, from: String, to: String): Map[String, Any] = {
    val fromPos = ChessXY.fromAn(from).get
    val toPos = ChessXY.fromAn(to).get
    val game = ChessGame.fromOstinatoString(ostinatoString).toOption
    val action = game flatMap (_.board.movementsOfDelta(fromPos, toPos - fromPos).headOption)

    moveResult(action, game)
  }

  def basicAiMove(fen: String, _depth: Int, _debug: Boolean): Map[String, Any] = {
    val game = ChessGame.fromOstinatoString(fen).toOption
    val action = game flatMap (instantiateChessBasicAi(game.get.board.turn, _depth, _debug).nextAction(_))

    moveResult(action, game)
  }

  protected def instantiateChessBasicAi(_player: ChessPlayer, _depth: Int, _debug: Boolean) =
    ChessBasicAi(player = _player, debug = _debug, depth = _depth)

  def randomAiMove(fen: String): Map[String, Any] = {
    val game = ChessGame.fromOstinatoString(fen).toOption
    val action = game flatMap (ChessRandomAi(game.get.board.turn).nextNonFinalAction(_))

    moveResult(action, game)
  }

  private def moveResult(action: Option[ChessAction], game: Option[ChessGame]): Map[String, Any] = {
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
      )
    }) getOrElse Map("success" -> (false: Any))
  }

  def parseNotation(input: String): Map[String, Any] = {
    val results = NotationParser.parseMatchString(input).results

    results.head match {
      case parsedMatch @ ParsedMatch(steps, notationRules) ⇒
        val boards = steps.filter(_.maybeGameStep.nonEmpty).map(_.maybeGameStep.get.board.toOstinatoString)

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
          "boards" -> boards.toArray,
          "actions" -> actions.toArray,
          "validActionCount" -> validActionCount,
          "parseWasSuccessful" -> parseWasSuccessful,
          "notationName" -> notationName
        )
    }
  }

  def convertNotation(input: String, notation: String): Map[String, Any] = {
    val results = NotationParser.parseMatchString(input)

    Map(
      "actions" ->
        results.parsedMatches.head.flatMap(
          _.maybeGameStep.map(
            gameStep ⇒ getActionParser(notation).serialiseAction(gameStep.action).head._1
          )
        ).toArray,
      "validActionCount" -> results.results.head.validStepCount
    )
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
