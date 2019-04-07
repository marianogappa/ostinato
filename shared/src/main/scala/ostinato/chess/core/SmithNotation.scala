package ostinato.chess.core

import ostinato.chess.core.NotationParser.PreParseInsights

case class SmithNotationRules() extends NotationRules {
  val shortName = "Smith Notation"
  val fullName = "Smith Notation"
}

object SmithNotation extends Notation[SmithNotationRules] {
  def allPossibleRules: Set[SmithNotationRules] = Set(SmithNotationRules())
}

case class SmithNotationActionSerialiser(r: SmithNotationRules)
    extends ActionSerialiser {
  protected def move(a: MoveAction, i: PreParseInsights) = fromPos(a) * toPos(a)

  protected def enPassant(a: EnPassantAction, i: PreParseInsights) = fromPos(a) * toPos(a)

  protected def capture(a: CaptureAction, i: PreParseInsights) =
    fromPos(a) * toPos(a) * a.toPiece.toDn.map(_.toLowerCase)

  protected def enPassantCapture(a: EnPassantCaptureAction, i: PreParseInsights) =
    fromPos(a) * toPos(a) * a.toPawn.toDn.map(_.toLowerCase)

  protected def castling(a: CastlingAction, i: PreParseInsights) =
    fromPos(a) * toPos(a) * (if (a.isKingside) "c" else "C")

  protected def promote(a: PromoteAction, i: PreParseInsights) =
    fromPos(a) * toPos(a) * a.promotePiece.toAn

  protected def lose(a: LoseAction, i: PreParseInsights) = Set("")

  protected def draw(a: DrawAction, i: PreParseInsights) = Set("")

  private def fromPos(a: ChessAction) = a.fromPiece.pos.toAn.toString

  private def toPos(a: ChessAction) = (a.fromPiece.pos + a.delta).toAn.toString

  protected def capturePromote(a: CapturePromoteAction, i: PreParseInsights) =
    fromPos(a) * toPos(a) * a.capturedPiece.toDn
      .map(_.toLowerCase) * a.promotePiece.toAn
}
