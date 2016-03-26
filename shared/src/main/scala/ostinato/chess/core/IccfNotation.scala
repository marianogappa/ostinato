package ostinato.chess.core

case class IccfNotationRules() extends NotationRules {
  val shortName = "ICCF Notation"
  val fullName = "ICCF Notation"
}

object IccfNotation extends Notation[IccfNotationRules] {
  def allPossibleRules: Set[IccfNotationRules] = Set(IccfNotationRules())
}

case class IccfNotationActionParser(r: IccfNotationRules = IccfNotationRules()) extends ActionParser {
  protected def move(a: MoveAction) = action(a)
  protected def enPassant(a: EnPassantAction) = action(a)
  protected def capture(a: CaptureAction) = action(a)
  protected def enPassantCapture(a: EnPassantCaptureAction) = action(a)
  protected def castling(a: CastlingAction)(implicit rules: ChessOptimisations = ChessOptimisations.default) = action(a)
  protected def promote(a: PromoteAction) = action(a) * toPiece(a)
  protected def lose(a: LoseAction) = Set("")
  protected def draw(a: DrawAction) = Set("")
  protected def capturePromote(a: CapturePromoteAction) = action(a) * toPiece(a)

  private def action(a: ChessAction) = fromPos(a) * toPos(a)
  private def fromPos(a: ChessAction) = a.fromPiece.pos.toIccf.toString
  private def toPos(a: ChessAction) = (a.fromPiece.pos + a.delta).toIccf.toString
  private def toPiece(a: PromoteAction) = a.toPiece.toIccf.toString
  private def toPiece(a: CapturePromoteAction) = a.promotePiece.toIccf.toString
}
