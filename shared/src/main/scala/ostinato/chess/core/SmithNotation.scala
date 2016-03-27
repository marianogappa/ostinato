package ostinato.chess.core

case class SmithNotationRules() extends NotationRules {
  val shortName = "Smith Notation"
  val fullName = "Smith Notation"
}

object SmithNotation extends Notation[SmithNotationRules] {
  def allPossibleRules: Set[SmithNotationRules] = Set(SmithNotationRules())
}

case class SmithNotationActionSerialiser(r: SmithNotationRules) extends ActionSerialiser {
  protected def move(a: MoveAction) = fromPos(a) * toPos(a)
  protected def enPassant(a: EnPassantAction) = fromPos(a) * toPos(a)
  protected def capture(a: CaptureAction) = fromPos(a) * toPos(a) * a.toPiece.toDn.map(_.toLowerCase)
  protected def enPassantCapture(a: EnPassantCaptureAction) = fromPos(a) * toPos(a) * a.toPawn.toDn.map(_.toLowerCase)
  protected def castling(a: CastlingAction) = fromPos(a) * toPos(a) * (if (a.isKingside) "c" else "C")
  protected def promote(a: PromoteAction) = fromPos(a) * toPos(a) * a.promotePiece.toAn
  protected def lose(a: LoseAction) = Set("")
  protected def draw(a: DrawAction) = Set("")
  private def fromPos(a: ChessAction) = a.fromPiece.pos.toAn.toString
  private def toPos(a: ChessAction) = (a.fromPiece.pos + a.delta).toAn.toString

  protected def capturePromote(a: CapturePromoteAction) =
    fromPos(a) * toPos(a) * a.capturedPiece.toDn.map(_.toLowerCase) * a.promotePiece.toAn
}
