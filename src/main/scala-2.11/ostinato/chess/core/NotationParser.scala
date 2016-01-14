package ostinato.chess.core

import scala.annotation.tailrec

object NotationParser {
  type ParsedMatch = (List[ParseStep], ParseResult)
  type ParseStep = (String, Option[(ChessAction, ChessBoard)])
  type ParseResult = Either[Option[NotationRules], NotationRules]

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
        Set((steps, Right(actionParser.r)))
      case a :: as ⇒
        val nodes = cache.getOrElse(currentBoard, store(currentBoard)) flatMap actionParser.parseAction filter (_._1 == a)

        if (nodes.isEmpty) {
          Set((steps ++ (a :: as).map((_, None)), Left(Some(actionParser.r))))
        } else {
          reduce {
            nodes.flatMap[ParsedMatch, Set[ParsedMatch]] {
              case (_: String, (chessAction: ChessAction, notationRules: NotationRules)) ⇒
                currentBoard.doAction(chessAction) match {
                  case Some(newBoard: ChessBoard) ⇒
                    //                                    println(s"Successfully processed $a with $chessAction", newBoard)
                    doParseMatch(as, newBoard, steps :+ (a, Some((chessAction, newBoard))), actionParser)
                  case None ⇒
                    val allSteps: List[ParseStep] = steps ++ (a :: as).map((_, None))
                    Set((allSteps, Left(Some(notationRules))))
                }
            }
          }
        }
    }

  private def reduce(results: Set[ParsedMatch]): Set[ParsedMatch] = {
    lazy val hasSuccess = results exists (_._2.isRight)
    lazy val removeFailures = results filter (_._2.isRight)

    def validSteps(steps: List[ParseStep]) = steps.count(_._2.nonEmpty)

    lazy val leaveBestAttempts = {
      val sorted = results.toList.sortWith((a, b) ⇒ validSteps(a._1) > validSteps(b._1))
      val targetSize = validSteps(sorted.head._1)
      sorted.takeWhile(r ⇒ validSteps(r._1) == targetSize).toSet
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
      case _ if partialResults exists (_._2.isRight) ⇒
        ParseResultsProxy(reduce(partialResults))
      case actionParser :: as ⇒
        parseMatchString(s, board, as,
          partialResults ++ doParseMatch(prepareMatchString(s).toList, board, List.empty[ParseStep], actionParser))
    }

  case class ParseResultsProxy(results: Set[ParsedMatch]) {
    val isEmpty = results.isEmpty
    val succeeded = results exists (_._2.isRight)
    val failed = !succeeded
    lazy val suceedingNotations = if (succeeded) results collect { case (_, Right(notation)) ⇒ notation } else Set()
    lazy val failingNotations = if (failed) results collect { case (_, Left(Some(notation))) ⇒ notation } else Set()
    lazy val parsedMatches = results map (_._1)
  }
}

