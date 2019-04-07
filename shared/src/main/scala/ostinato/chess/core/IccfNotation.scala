package ostinato.chess.core

import scala.util.{Failure, Success, Try}
import scala.util.control.NoStackTrace

import ostinato.chess.core.NotationParser.PreParseInsights

case class IccfNotationRules() extends NotationRules {
  val shortName = "ICCF Notation"
  val fullName = "ICCF Notation"
}

object IccfNotation extends Notation[IccfNotationRules] {
  def allPossibleRules: Set[IccfNotationRules] = Set(IccfNotationRules())

  private def possibleGameSteps(s: String,
                                board: ChessBoard): Try[List[GameStep]] = {
    val List(fromTry, toTry) =
      s.grouped(2).toList.map(s => IccfPos.fromString(s).map(_.toXY))
    val pgs = for {
      from <- fromTry
      to <- toTry
    } yield {
      board.movementsOfDelta(from, to - from) flatMap {
        case action =>
          board.doAction(action)(ChessOptimisations.noCheckForThreatens) map (GameStep(
            Some(action),
            _))
      }
    }
    pgs.map(_.toList)
  }

  private def assertOneAction(possibleGameSteps: Try[List[GameStep]],
                              s: String): Try[GameStep] = {
    possibleGameSteps match {
      case Success(List(gameStep)) => Success(gameStep)
      case Success(a) =>
        Failure(InvalidIccfActionString(s, IccfActionStringAmbiguous))
      case Failure(e) => Failure(InvalidIccfActionString(s, e))
    }
  }

  def parseActionString(s: String, board: ChessBoard): Try[GameStep] =
    s.length match {
      case 4 =>
        assertOneAction(possibleGameSteps(s, board), s)
      case 5 =>
        val List(coordinates, promotePieceCode) = s.grouped(4).toList
        val filteredPossibleGameSteps =
          possibleGameSteps(coordinates, board).map {
            case gameStepList =>
              gameStepList collect {
                case gs @ GameStep(Some(a: ActionWithPromotion), b)
                    if a.promotedPieceIccfCode == promotePieceCode =>
                  gs
              }
          }
        assertOneAction(filteredPossibleGameSteps, s)
      case _ => Failure(InvalidIccfActionString(s, IccfActionStringTooLong))
    }
}

case class IccfNotationActionSerialiser(
    r: IccfNotationRules = IccfNotationRules())
    extends ActionSerialiser {
  protected def move(a: MoveAction, i: PreParseInsights) = action(a)

  protected def enPassant(a: EnPassantAction, i: PreParseInsights) = action(a)

  protected def capture(a: CaptureAction, i: PreParseInsights) = action(a)

  protected def enPassantCapture(a: EnPassantCaptureAction, i: PreParseInsights) = action(a)

  protected def castling(a: CastlingAction, i: PreParseInsights) = action(a)

  protected def promote(a: PromoteAction, i: PreParseInsights) = action(a) * toPiece(a)

  protected def lose(a: LoseAction, i: PreParseInsights) = Set("")

  protected def draw(a: DrawAction, i: PreParseInsights) = Set("")

  protected def capturePromote(a: CapturePromoteAction, i: PreParseInsights) =
    action(a) * toPiece(a)

  private def action(a: ChessAction) = fromPos(a) * toPos(a)

  private def fromPos(a: ChessAction) = a.fromPiece.pos.toIccf.toString

  private def toPos(a: ChessAction) =
    (a.fromPiece.pos + a.delta).toIccf.toString

  private def toPiece(a: PromoteAction) = a.promotePiece.toIccf.toString

  private def toPiece(a: CapturePromoteAction) = a.promotePiece.toIccf.toString
}

case object IccfActionStringAmbiguous
    extends RuntimeException(
      "ICCF actions should be parsed to exactly one action")
    with NoStackTrace

case object IccfActionStringTooLong
    extends RuntimeException("ICCF actions must be 5 chars long at most")
    with NoStackTrace

case class InvalidIccfActionString(s: String, e: Throwable)
    extends RuntimeException(s"Invalid ICCF string: [$s] given:", e)
    with NoStackTrace
