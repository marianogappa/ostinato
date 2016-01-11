package ostinato.chess.core

object NotationParser {
  type ParsedMatch = (List[ParseStep], ParseResult)
  type ParseStep = (String, Option[(ChessAction, ChessBoard)])
  type ParseResult = Either[Option[NotationRules], NotationRules]

  private def prepareMatchString(s: String) =
    s.replaceAll("""\s+|\d+\.|\[[^\]]*\]""", " ").replaceAll(" +", " ").replaceAll("""[\?!]*""", "").trim.split(' ')

  private def doParseMatch(actions: List[String], currentBoard: ChessBoard, steps: List[ParseStep], actionParser: ActionParser)(
    implicit rules: ChessRules = ChessRules.default): Set[ParsedMatch] =
    actions match {
      case Nil ⇒
        Set((steps, Right(actionParser.r)))
      case a :: as ⇒
        val nodes = currentBoard.actions flatMap actionParser.parseAction filter (_._1 == a)

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
      val sorted = results.toList.sortWith( (a, b) => validSteps(a._1) > validSteps(b._1))
      val targetSize = validSteps(sorted.head._1)
      sorted.takeWhile(r => validSteps(r._1) == targetSize).toSet
    }

    if (results.isEmpty)
      Set()
    else if (hasSuccess)
      removeFailures
    else
      leaveBestAttempts
  }

  private def allActionParsers: Set[ActionParser] = {
    val dap = DescriptiveNotation.allPossibleRules map DescriptiveNotationActionParser
    val cap = CoordinateNotation.allPossibleRules map CoordinateNotationActionParser
    val sap = SmithNotation.allPossibleRules map SmithNotationActionParser
    val aap = AlgebraicNotation.allPossibleRules map AlgebraicNotationActionParser
    val iap = IccfNotation.allPossibleRules map IccfNotationActionParser

    dap ++ cap ++ sap ++ aap ++ iap
  }

  def parseMatchString(s: String, board: ChessBoard = ChessGame.defaultGame.board)(implicit rules: ChessRules = ChessRules.default) =
    ParseResultsProxy(reduce(allActionParsers.flatMap(doParseMatch(prepareMatchString(s).toList, board, List.empty[ParseStep], _))))

  case class ParseResultsProxy(results: Set[ParsedMatch]) {
    val isEmpty = results.isEmpty
    val succeeded = results exists (_._2.isRight)
    val failed = !succeeded
    lazy val suceedingNotations = if (succeeded) results collect { case (_, Right(notation)) ⇒ notation } else Set()
    lazy val failingNotations = if (failed) results collect { case (_, Left(Some(notation))) ⇒ notation } else Set()
    lazy val parsedMatches = results map (_._1)
  }
}

