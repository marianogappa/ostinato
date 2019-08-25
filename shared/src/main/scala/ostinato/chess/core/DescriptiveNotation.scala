package ostinato.chess.core

import ostinato.chess.core.NotationParser.PreParseInsights

case class DescriptiveNotationRules(omitDash: Boolean,
                                    numericalRankBeforeFile: Boolean,
                                    omitFirstRank: Boolean,
                                    castlingNotation: String)
    extends NotationRules {

  val shortName = "Descriptive Notation"
  val fullName =
    s"""Descriptive Notation (using ${if (omitDash) "omitting"
       else "not omitting"} dashes,
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
    } yield
      DescriptiveNotationRules(omitDash,
                               numericalRankBeforeFile,
                               omitFirstRank,
                               castlingNotation)
  }
}

case class DescriptiveNotationActionSerialiser(r: DescriptiveNotationRules)
    extends ActionSerialiser {
  protected def move(a: MoveAction, i: PreParseInsights): Set[String] =
    (fromPiece(a) * "-" * toPos(a) * checkAndCheckmate(a) * moveQuality(i)) ++
      (simplePieceDn(a.fromPiece) * "(" * parensPos(a.fromPiece.pos, a.turn) * ")" * "-" * toPos(a) * checkAndCheckmate(a) * moveQuality(i)) // This line is edge case: P(QR2)

  protected def enPassant(a: EnPassantAction, i: PreParseInsights): Set[String] = // same as move
    (fromPiece(a) * "-" * toPos(a) * checkAndCheckmate(a) * moveQuality(i)) ++
      (simplePieceDn(a.fromPiece) * "(" * parensPos(a.fromPiece.pos, a.turn) * ")" * "-" * toPos(a) * checkAndCheckmate(a) * moveQuality(i)) // This line is edge case: P(QR2)

  protected def capture(a: CaptureAction, i: PreParseInsights): Set[String] =
    (fromPiece(a) * "x" * toCapturedPiece(a) * checkAndCheckmate(a) * moveQuality(i)) ++
      (fromPiece(a) * "x" * toCapturedPiece(a) * "/" * parensPos(a.toPiece.pos, a.turn) * checkAndCheckmate(a) * moveQuality(i)) // This line is edge case: BxN/QB6

  protected def castling(a: CastlingAction, i: PreParseInsights): Set[String] =
    castlingSymbol(a) * checkAndCheckmate(a) * moveQuality(i)

  protected def lose(a: LoseAction, i: PreParseInsights): Set[String] =
    if (a.player == WhiteChessPlayer) Set("0-1", "resigns") else Set("1-0", "resigns")

  protected def draw(a: DrawAction, i: PreParseInsights): Set[String] = Set("1/2-1/2")

  private def fromPiece(a: ChessAction): Set[String] =
    a.fromPiece.toDn * Set("", fromPieceRank(a.fromPiece))

  private def toCapturedPiece(a: CaptureAction): Set[String] =
    a.toPiece.toDn * Set("", fromPieceRank(a.fromPiece))

  protected def enPassantCapture(a: EnPassantCaptureAction, i: PreParseInsights): Set[String] =
    fromPiece(a) * "x" * a.toPawn.toDn * Set("e.p.", "") * checkAndCheckmate(a) * moveQuality(i)

  //TODO review this!
  protected def promote(a: PromoteAction, i: PreParseInsights): Set[String] =
    fromPiece(a) * "-" * toPos(a) * genericPromotion(a.promotePiece) * checkAndCheckmate(a) * moveQuality(i)

  protected def capturePromote(a: CapturePromoteAction, i: PreParseInsights): Set[String] =
    fromPiece(a) * "x" * a.capturedPiece.toDn * genericPromotion(
      a.promotePiece) * checkAndCheckmate(a) * moveQuality(i)

  private def toPos(a: ChessAction): Set[String] =
    (a.fromPiece.pos + a.delta).toDn(a.turn).map {
      case DnPos(file, 1) if r.omitFirstRank ⇒ file // https://en.wikipedia.org/wiki/Descriptive_notation#Notation_for_moves
      case p @ DnPos(file, rank) ⇒
        if (r.numericalRankBeforeFile) rank.toString + file else p.toString
    }

  private def simplePieceDn(p: ChessPiece): Set[String] = // Used for this case: P(QR2). It's the part before parens.
    p match {
      case _ if p.isRook => Set("R")
      case _ if p.isKnight => Set("N", "Kt")
      case _ if p.isBishop => Set("B")
      case _ if p.isKing => Set("K")
      case _ if p.isQueen => Set("Q")
      case _ if p.isPawn => Set("P")
    }

  private def parensPos(xy: ChessXY, turn: ChessPlayer): Set[String] = { // Used for this case: P(QR2). It's the part in parens
    val dnPoses: Set[DnPos] = xy.toDn(turn) // All possible denominations for fromPos in a (x, y) tuple

    // Can omit rank, omit file, omit none, be rank file or file rank. Kill me.
    dnPoses.map(_.x) ++
      dnPoses.map(_.y.toString) ++
      dnPoses.map{ case DnPos(x, y) => s"$x$y" } ++
      dnPoses.map{ case DnPos(x, y) => s"$y$x" }
  }

  private def fromPieceRank(p: ChessPiece): String =
    (if (p.owner == BlackChessPlayer) p.pos.y+1 else 8-p.pos.y).toString

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
    else if (a.isKingside) Set("O-O", "0-0")
    else Set("O-O-O", "0-0-0")

  private def moveQuality(i: PreParseInsights): Set[String] = {
    val qualifier = i match {
      case pi if pi.good ⇒ "!"
      case pi if pi.excellent ⇒ "!!"
      case pi if pi.poor ⇒ "?"
      case pi if pi.blunder ⇒ "??"
      case pi if pi.maybeBad ⇒ "?!"
      case pi if pi.maybeGood ⇒ "!?"
      case _ ⇒ ""
    }
    Set("", qualifier)
  }

  override def prepareMatchString(s: String): List[(String, PreParseInsights)] = {
    def preParseInsights(s: String) = PreParseInsights(
      good = s.matches("""^[^!?]*![^!?]*$"""),
      excellent = s.matches(""".*!!.*"""),
      poor = s.matches("""[^?]*\?[^?]*"""),
      blunder = s.matches(""".*\?\?.*"""),
      maybeGood = s.matches(""".*!\?.*"""),
      maybeBad = s.matches(""".*\?!.*""")
    )
    s.toUpperCase.replaceAll(""" CH""", "CH")
      .replaceAll(""" ?DIS[. ]{0,2}CH\.?""", "CH")
      .replaceAll(""" ?DBLCH""", "CH")
      .replaceAll(""" MATE""", "MATE")
      .replaceAll("""\s+|\d+\.|\[[^\]]*\]""", " ")
      .replaceAll(" +", " ")
      .trim
      .split(' ')
      .toList
      .map(actionString => (actionString, preParseInsights(actionString)))
      .map(st => (st._1.replaceAll("""[\?!]*""", ""), st._2))
  }
}
