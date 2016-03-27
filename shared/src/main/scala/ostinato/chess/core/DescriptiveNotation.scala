package ostinato.chess.core

case class DescriptiveNotationRules(
  omitDash: Boolean,
  numericalRankBeforeFile: Boolean,
  omitFirstRank: Boolean,
  castlingNotation: String) extends NotationRules {

  val shortName = "Descriptive Notation"
  val fullName =
    s"""Descriptive Notation (using ${if (omitDash) "omitting" else "not omitting"} dashes,
        | ${if (numericalRankBeforeFile) "using" else "not using"} numerical rank before file,
        | ${if (omitFirstRank) "omitting" else "not omitting"} first rank, using $castlingNotation for
        | castling notation)
     """.stripMargin
}

object DescriptiveNotation extends Notation[DescriptiveNotationRules] {
  def allPossibleRules: Set[DescriptiveNotationRules] = {
    for {
      omitDash ← Set(true, false)
      numericalRankBeforeFile ← Set(true, false)
      omitFirstRank ← Set(true, false)
      castlingNotation ← Set("zeroes", "word")
    } yield DescriptiveNotationRules(omitDash, numericalRankBeforeFile, omitFirstRank, castlingNotation)
  }
}

case class DescriptiveNotationActionSerialiser(r: DescriptiveNotationRules) extends ActionSerialiser {
  protected def move(a: MoveAction): Set[String] = fromPiece(a) * dash(a) * toPos(a) * checkAndCheckmate(a)
  protected def enPassant(a: EnPassantAction): Set[String] = fromPiece(a) * dash(a) * toPos(a) * checkAndCheckmate(a)
  protected def capture(a: CaptureAction): Set[String] = fromPiece(a) * "x" * a.toPiece.toDn * checkAndCheckmate(a)
  protected def castling(a: CastlingAction): Set[String] = castlingSymbol(a) * checkAndCheckmate(a)
  protected def lose(a: LoseAction): Set[String] = if (a.player == WhiteChessPlayer) Set("0-1") else Set("1-0")
  protected def draw(a: DrawAction): Set[String] = Set("1/2-1/2")
  private def fromPiece(a: ChessAction): Set[String] = a.fromPiece.toDn
  private def dash(a: ChessAction): Set[String] = if (r.omitDash) Set() else Set("-")

  protected def enPassantCapture(a: EnPassantCaptureAction): Set[String] =
    fromPiece(a) * "x" * a.toPawn.toDn * checkAndCheckmate(a)

  //TODO review this!
  protected def promote(a: PromoteAction): Set[String] =
    fromPiece(a) * genericPromotion(a.promotePiece) * checkAndCheckmate(a)

  protected def capturePromote(a: CapturePromoteAction): Set[String] =
    fromPiece(a) * "x" * a.capturedPiece.toDn * genericPromotion(a.promotePiece) * checkAndCheckmate(a)

  private def toPos(a: ChessAction): Set[String] =
    (a.fromPiece.pos + a.delta).toDn(a.turn).map {
      case DnPos(file, 1) if r.omitFirstRank ⇒ file
      case p @ DnPos(file, rank)                 ⇒ if (r.numericalRankBeforeFile) rank.toString + file else p.toString
    }

  private def checkAndCheckmate(a: ChessAction): Set[String] =
    if (a.isCheckmate)
      Set("++", "mate")
    else if (a.isCheck)
      Set("+", "ch")
    else
      Set("")

  private def castlingSymbol(a: CastlingAction): Set[String] =
    if (r.castlingNotation == "word")
      Set("castles", "Castles")
    else if (a.isKingside) Set("O-O", "0-0") else Set("O-O-O", "0-0-0")
}
