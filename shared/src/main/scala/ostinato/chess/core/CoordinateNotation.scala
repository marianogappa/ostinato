package ostinato.chess.core

case class CoordinateNotationRules(lowerCaseLetters: Boolean,
                                   useDashDelimiter: Boolean,
                                   distinguishCaptures: Boolean,
                                   castlingNotation: String)
    extends NotationRules {

  val shortName = "Coordinate Notation"
  val fullName =
    s"""Coordinate Notation (using ${if (lowerCaseLetters) "lowercase"
       else "uppercase"} letters for files,
       | ${if (useDashDelimiter) "using" else "not using"} dashes as delimiters,
       | ${if (distinguishCaptures) "distinguishing" else "not distinguishing"} captures, using $castlingNotation for
       | castling notation)
     """.stripMargin
}

object CoordinateNotation extends Notation[CoordinateNotationRules] {
  def allPossibleRules: Set[CoordinateNotationRules] =
    for {
      lowerCaseLetters ← Set(true, false)
      useDashDelimiter ← Set(true, false)
      distinguishCaptures ← Set(true, false)
      castlingNotation ← Set("zeroes", "os", "word")
    } yield
      CoordinateNotationRules(lowerCaseLetters,
                              useDashDelimiter,
                              distinguishCaptures,
                              castlingNotation)
}

case class CoordinateNotationActionSerialiser(r: CoordinateNotationRules)
    extends ActionSerialiser {

  protected def lose(a: LoseAction): Set[String] =
    if (a.player == WhiteChessPlayer) Set("0-1") else Set("1-0")

  protected def draw(a: DrawAction): Set[String] =
    Set("1/2-1/2", "½–½", "draws")

  protected def capture(a: CaptureAction) =
    fromPos(a) * captureDash * toPos(a) * checkAndCheckmate(a)

  protected def enPassantCapture(a: EnPassantCaptureAction) =
    fromPos(a) * captureDash * toPos(a) * checkAndCheckmate(a)

  protected def move(a: MoveAction) = action(a)

  protected def enPassant(a: EnPassantAction) = action(a)

  protected def action(a: ChessAction) =
    fromPos(a) * dash * toPos(a) * checkAndCheckmate(a)

  protected def promote(a: PromoteAction) =
    fromPos(a) * dash * toPos(a) * genericPromotion(a.promotePiece) * checkAndCheckmate(
      a)

  protected def capturePromote(a: CapturePromoteAction) =
    fromPos(a) * captureDash * toPos(a) * genericPromotion(a.promotePiece) * checkAndCheckmate(
      a)

  protected def castling(a: CastlingAction): Set[String] =
    r.castlingNotation match {
      case "zeroes" if a.isKingside ⇒ Set("0-0")
      case "zeroes" if a.isQueenside ⇒ Set("0-0-0")
      case "os" if a.isKingside ⇒ Set("O-O")
      case "os" if a.isQueenside ⇒ Set("O-O-O")
      case "word" ⇒ Set("castles")
    }

  private def dash = if (r.useDashDelimiter) Set("-") else Set("")

  private def captureDash =
    if (r.useDashDelimiter) if (r.distinguishCaptures) Set("x") else Set("-")
    else Set("")

  private def fromPos(a: ChessAction): Set[String] =
    if (r.lowerCaseLetters)
      Set(a.fromPiece.pos.toAn.toString.toLowerCase)
    else
      Set(a.fromPiece.pos.toAn.toString.toUpperCase)

  private def toPos(a: ChessAction): Set[String] =
    Set((a.fromPiece.pos + a.delta).toAn.toString.toLowerCase,
        (a.fromPiece.pos + a.delta).toAn.toString.toUpperCase)

  private def checkAndCheckmate(a: ChessAction): Set[String] =
    if (a.isCheckmate)
      Set("++", "mate")
    else if (a.isCheck)
      Set("+", "ch")
    else
      Set("")
}
