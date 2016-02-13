package ostinato.chess.core

import scala.annotation.tailrec

object NotationParser {
  case class GameStep(action: ChessAction, board: ChessBoard)
  case class ParseStep(stringToParse: String, maybeGameStep: Option[GameStep])
  abstract class ParsingResult {
    val isSuccess: Boolean
  }
  case class FailedParse(nr: Option[NotationRules]) extends ParsingResult {
    val isSuccess = false
  }
  case class SuccessfulParse(nr: NotationRules) extends ParsingResult {
    val isSuccess = true
  }
  case class ParsedMatch(steps: List[ParseStep], result: ParsingResult) {
    val isSuccess = result.isSuccess
    val validStepCount = steps count (_.maybeGameStep.nonEmpty)
  }

  private val cache: collection.mutable.Map[ChessBoard, Set[ChessAction]] = collection.mutable.Map.empty[ChessBoard, Set[ChessAction]]
  private def store(board: ChessBoard)(implicit rules: ChessRules = ChessRules.default) = {
    val actions = board.actions
    cache(board) = actions
    actions
  }

  private def prepareMatchString(s: String) =
    s.replaceAll("""\s+|\d+\.|\[[^\]]*\]""", " ").replaceAll(" +", " ").replaceAll("""[\?!]*""", "").trim.split(' ')

  private def doParseMatch(actions: List[String], currentBoard: ChessBoard, steps: List[ParseStep], actionParser: ActionParser)(
    implicit rules: ChessRules = ChessRules.default): Set[ParsedMatch] =
    actions match {
      case Nil ⇒
        Set(ParsedMatch(steps, SuccessfulParse(actionParser.r)))
      case a :: as ⇒
        val nodes = cache.getOrElse(currentBoard, store(currentBoard)) flatMap actionParser.parseAction filter (_._1 == a)

        if (nodes.isEmpty) {
          Set(ParsedMatch(steps ++ (a :: as).map(ParseStep(_, None)), FailedParse(Some(actionParser.r))))
        } else {
          reduce {
            nodes.flatMap {
              case (_: String, (chessAction: ChessAction, notationRules: NotationRules)) ⇒
                currentBoard.doAction(chessAction) match {
                  case Some(newBoard: ChessBoard) ⇒
                    //                                    println(s"Successfully processed $a with $chessAction", newBoard)
                    doParseMatch(as, newBoard, steps :+ ParseStep(a, Some(GameStep(chessAction, newBoard))), actionParser)
                  case None ⇒
                    val allSteps: List[ParseStep] = steps ++ (a :: as).map(ParseStep(_, None))
                    Set(ParsedMatch(allSteps, FailedParse(Some(notationRules))))
                }
            }
          }
        }
    }

  private def reduce(results: Set[ParsedMatch]): Set[ParsedMatch] = {
    lazy val hasSuccess = results exists (_.isSuccess)
    lazy val removeFailures = results filter (_.isSuccess)

    lazy val leaveBestAttempts = {
      val sorted = results.toList.sortWith(_.validStepCount > _.validStepCount)
      val targetSize = sorted.head.validStepCount
      sorted.takeWhile(_.validStepCount == targetSize).toSet
    }

    if (results.isEmpty)
      Set()
    else if (hasSuccess)
      removeFailures
    else
      leaveBestAttempts
  }

  private def allActionParsers: List[ActionParser] = {
    (AlgebraicNotation.allPossibleRules map AlgebraicNotationActionParser).toList ++
      (SmithNotation.allPossibleRules map SmithNotationActionParser).toList ++
      (IccfNotation.allPossibleRules map IccfNotationActionParser).toList ++
      (DescriptiveNotation.allPossibleRules map DescriptiveNotationActionParser).toList ++
      (CoordinateNotation.allPossibleRules map CoordinateNotationActionParser).toList
  }

  @tailrec
  def parseMatchString(
    s: String,
    board: ChessBoard = ChessGame.defaultGame.board,
    actionParsers: List[ActionParser] = allActionParsers,
    partialResults: Set[ParsedMatch] = Set())(
      implicit rules: ChessRules = ChessRules.default): ParseResultsProxy =
    actionParsers match {
      case Nil ⇒
        ParseResultsProxy(reduce(partialResults))
      case _ if partialResults exists (_.isSuccess) ⇒
        ParseResultsProxy(reduce(partialResults))
      case actionParser :: as ⇒
        parseMatchString(s, board, as,
          partialResults ++ doParseMatch(prepareMatchString(s).toList, board, List.empty[ParseStep], actionParser))
    }

  case class ParseResultsProxy(results: Set[ParsedMatch]) {
    val isEmpty = results.isEmpty
    val succeeded = results exists (_.isSuccess)
    val failed = !succeeded
    lazy val suceedingNotations = if (succeeded) results collect { case ParsedMatch(_, SuccessfulParse(notation)) ⇒ notation } else Set()
    lazy val failingNotations = if (failed) results collect { case ParsedMatch(_, FailedParse(Some(notation))) ⇒ notation } else Set()
    lazy val parsedMatches = results map (_.steps)
  }
}

