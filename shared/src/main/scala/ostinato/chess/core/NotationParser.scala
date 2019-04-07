package ostinato.chess.core

import scala.annotation.tailrec

object NotationParser {

  case class PreParseInsights(
                               good: Boolean = false,
                               excellent: Boolean = false,
                               poor: Boolean = false,
                               blunder: Boolean = false,
                               maybeGood: Boolean = false,
                               maybeBad: Boolean = false
                             )

  case class ParseStep(stringToParse: String, maybeGameStep: Option[GameStep], preParseInsights: PreParseInsights)

  case class GameStep(action: ChessAction, board: ChessBoard)

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
    val actionStrings = steps.map(_.stringToParse)
  }

  private val cache: collection.mutable.Map[ChessBoard, Set[ChessAction]] =
    collection.mutable.Map.empty[ChessBoard, Set[ChessAction]]

  private def store(board: ChessBoard)(implicit opts: ChessOptimisations =
                                         ChessOptimisations.default) = {
    val actions = board.actions
    cache(board) = actions
    actions
  }

  private def doParseMatch(actions: List[(String, PreParseInsights)],
                           currentBoard: ChessBoard,
                           steps: List[ParseStep],
                           actionSerialiser: ActionSerialiser)(
      implicit opts: ChessOptimisations = ChessOptimisations.default)
    : Set[ParsedMatch] =
    actions match {
      case Nil ⇒
        Set(ParsedMatch(steps, SuccessfulParse(actionSerialiser.r)))
      case a :: as ⇒
        val nodes = cache.getOrElse(currentBoard, store(currentBoard)) flatMap { case action: ChessAction ⇒
          actionSerialiser.serialiseAction(action, a._2) filter (_._1.toUpperCase == a._1)
        }

        if (nodes.isEmpty) {
          Set(
            ParsedMatch(steps ++ (a :: as).map(a ⇒ ParseStep(a._1, None, a._2)),
                        FailedParse(Some(actionSerialiser.r))))
        } else {
          reduce {
            nodes.flatMap {
              case (_: String,
                    (chessAction: ChessAction, notationRules: NotationRules)) ⇒
                currentBoard.doAction(chessAction) match {
                  case Some(newBoard: ChessBoard) ⇒
                    doParseMatch(as,
                                 newBoard,
                                 steps :+ ParseStep(
                                   a._1,
                                   Some(GameStep(chessAction, newBoard)),
                                   a._2),
                                 actionSerialiser)
                  case None ⇒
                    val allSteps: List[ParseStep] = steps ++ (a :: as).map(a ⇒
                      ParseStep(a._1, None, a._2))
                    Set(
                      ParsedMatch(allSteps, FailedParse(Some(notationRules))))
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

  private def allActionSerialisers: List[ActionSerialiser] = {
    (AlgebraicNotation.allPossibleRules map AlgebraicNotationActionSerialiser).toList ++
      (SmithNotation.allPossibleRules map SmithNotationActionSerialiser).toList ++
      (IccfNotation.allPossibleRules map IccfNotationActionSerialiser).toList ++
      (DescriptiveNotation.allPossibleRules map DescriptiveNotationActionSerialiser).toList ++
      (CoordinateNotation.allPossibleRules map CoordinateNotationActionSerialiser).toList
  }

  @tailrec
  def parseMatchString(
      s: String,
      board: ChessBoard = ChessGame.defaultGame.board,
      actionSerialisers: List[ActionSerialiser] = allActionSerialisers,
      partialResults: Set[ParsedMatch] = Set()): ParseResultsProxy =
    actionSerialisers match {
      case Nil ⇒
        ParseResultsProxy(reduce(partialResults))
      case _ if partialResults exists (_.isSuccess) ⇒
        ParseResultsProxy(reduce(partialResults))
      case actionSerialiser :: as ⇒
        parseMatchString(
          s,
          board,
          as,
          partialResults ++ doParseMatch(actionSerialiser.prepareMatchString(s),
                                         board,
                                         List.empty[ParseStep],
                                         actionSerialiser))
    }

  case class ParseResultsProxy(results: Set[ParsedMatch]) {
    val isEmpty = results.isEmpty
    val succeeded = results exists (_.isSuccess)
    val failed = !succeeded
    lazy val suceedingNotations = if (succeeded) results collect {
      case ParsedMatch(_, SuccessfulParse(notation)) ⇒ notation
    } else Set()
    lazy val failingNotations = if (failed) results collect {
      case ParsedMatch(_, FailedParse(Some(notation))) ⇒ notation
    } else Set()
    lazy val parsedMatches = results map (_.steps)
  }

}
