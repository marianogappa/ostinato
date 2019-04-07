package ostinato.chess.core

object CheckSymbol {
  val PLUS = "+"
  val CH = "ch"
  val CROSS = "†"
}

case class AlgebraicNotationRules(lowerCaseLetters: Boolean,
                                  figurine: Boolean,
                                  distinguishCaptures: Boolean,
                                  colonForCaptures: Boolean,
                                  castlingNotation: String,
                                  hashForCheckmate: Boolean,
                                  noFromPosForPawns: Boolean,
                                  checkSymbol: String,
                                  noFromPosOnCapturesExceptPawns: Boolean)
    extends NotationRules {

  val shortName = "Algebraic Notation"
  val fullName =
    s"""Algebraic Notation (using ${if (lowerCaseLetters) "lowercase"
       else "uppercase"} letters for files,
       | ${if (figurine) "using" else "not using"} figurines for pieces,
       | ${if (distinguishCaptures) "distinguishing" else "not distinguishing"} captures,
       | ${if (colonForCaptures) "using" else "not using"} colon for captures,
       | using $castlingNotation for
       | castling notation)
     """.stripMargin
}

object AlgebraicNotation extends Notation[AlgebraicNotationRules] {
  def allPossibleRules: Set[AlgebraicNotationRules] =
    for {
      lowerCaseLetters ← Set(true, false)
      figurine ← Set(true, false)
      distinguishCaptures ← Set(true, false)
      colonForCaptures ← Set(true, false)
      castlingNotation ← Set("zeroes", "os", "word")
      hashForCheckmate ← Set(true, false)
      noFromPosForPawns ← Set(true, false)
      checkSymbol ← Set(CheckSymbol.PLUS, CheckSymbol.CH, CheckSymbol.CROSS)
      noFromPosOnCapturesExceptPawns ← Set(true, false)
    } yield
      AlgebraicNotationRules(lowerCaseLetters,
                             figurine,
                             distinguishCaptures,
                             colonForCaptures,
                             castlingNotation,
                             hashForCheckmate,
                             noFromPosForPawns,
                             checkSymbol,
                             noFromPosOnCapturesExceptPawns)
}

case class AlgebraicNotationActionSerialiser(r: AlgebraicNotationRules)
    extends ActionSerialiser {

  protected def lose(a: LoseAction): Set[String] =
    if (a.player == WhiteChessPlayer) Set("0-1") else Set("1-0")

  protected def draw(a: DrawAction): Set[String] =
    Set("1/2-1/2", "½–½", "draws")

  protected def move(a: MoveAction) =
    fromPiece(a) * fromPos(a) * toPos(a) * checkAndCheckmate(a)

  protected def enPassant(a: EnPassantAction) =
    fromPiece(a) * fromPos(a) * toPos(a) * checkAndCheckmate(a)

  protected def promote(a: PromoteAction) =
    toPos(a) * promotion(a.promotePiece) * checkAndCheckmate(
      a)

  def promotion(toPiece: ChessPiece) =
    Set(
      s"=${toPiece.toAn.toString}",
      toPiece.toAn.toString,
      s"(${toPiece.toAn.toString})",
      s"/${toPiece.toAn.toString}"
    )

  protected def capture(a: CaptureAction) =
    fromPiece(a) * captureFromPos(a) * captureDash * toPos(a, withJustX = true) * checkAndCheckmate(
      a)

  protected def capturePromote(a: CapturePromoteAction) =
    fromPiece(a) *
      captureFromPos(a) * captureDash * toPos(a, withJustX = true) * genericPromotion(
      a.promotePiece) * checkAndCheckmate(a)

  protected def enPassantCapture(a: EnPassantCaptureAction) =
    a.fromPawn.pos.toAn.x.toString * (a.fromPawn.pos + a.delta).toAn.toString * Set(
      "e.p.",
      "") * checkAndCheckmate(a)

  protected def castling(a: CastlingAction): Set[String] =
    r.castlingNotation match {
      case "zeroes" if a.isKingside ⇒ Set("0-0")
      case "zeroes" if a.isQueenside ⇒ Set("0-0-0")
      case "os" if a.isKingside ⇒ Set("O-O")
      case "os" if a.isQueenside ⇒ Set("O-O-O")
      case "word" ⇒ Set("castles")
    }

  private def fromPiece(a: ChessAction) =
    if (r.figurine)
      if (a.fromPiece.isPawn) Set("") else Set(a.fromPiece.toFigurine.toString)
    else
      Set(a.fromPiece.toAn.toString)

  private def captureFromPos(a: ChessAction): Set[String] = {
    if (r.noFromPosOnCapturesExceptPawns && !a.fromPiece.isPawn) {
      return Set("")
    }
    fromPos(a)
  }

  private def fromPos(a: ChessAction) =
    if (r.noFromPosForPawns && a.fromPiece.isPawn)
      Set("")
    else if (r.lowerCaseLetters)
      Set("", a.fromPiece.pos.toAn.x.toString, a.fromPiece.pos.toAn.toString)
    else
      Set("",
          a.fromPiece.pos.toAn.x.toString.toUpperCase,
          a.fromPiece.pos.toAn.toString.toUpperCase)

  private def toPos(a: ChessAction, withJustX: Boolean = false): Set[String] =
    if (r.lowerCaseLetters)
      Set((a.fromPiece.pos + a.delta).toAn.toString.toLowerCase) ++
        (if (withJustX)
           Set((a.fromPiece.pos + a.delta).toAn.x.toString.toLowerCase)
         else Set())
    else
      Set((a.fromPiece.pos + a.delta).toAn.toString.toUpperCase) ++
        (if (withJustX)
           Set((a.fromPiece.pos + a.delta).toAn.x.toString.toUpperCase)
         else Set())

  private def checkAndCheckmate(a: ChessAction): Set[String] =
    if (a.isCheckmate)
      if (r.hashForCheckmate)
        Set("#")
      else
        Set("#", "++", "mate", "‡", "≠")
    else if (a.isCheck)
      Set(r.checkSymbol)
    else
      Set("")

  private def captureDash =
    if (r.distinguishCaptures) if (r.colonForCaptures) Set(":") else Set("x")
    else Set("")
}
