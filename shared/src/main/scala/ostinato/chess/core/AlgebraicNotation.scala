package ostinato.chess.core

import ostinato.chess.core.NotationParser.PreParseInsights

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

  protected def lose(a: LoseAction, i: PreParseInsights): Set[String] =
    if (a.player == WhiteChessPlayer) Set("0-1") else Set("1-0")

  protected def draw(a: DrawAction, i: PreParseInsights): Set[String] =
    Set("1/2-1/2", "½–½", "draws")

  protected def move(a: MoveAction, i: PreParseInsights) =
    fromPiece(a) * fromPos(a) * toPos(a) * checkAndCheckmate(a) * moveQuality(i)

  protected def enPassant(a: EnPassantAction, i: PreParseInsights) =
    fromPiece(a) * fromPos(a) * toPos(a) * checkAndCheckmate(a) * moveQuality(i)

  protected def promote(a: PromoteAction, i: PreParseInsights) =
    toPos(a) * promotion(a.promotePiece) * checkAndCheckmate(a) * moveQuality(i)

  def promotion(toPiece: ChessPiece) =
    Set(
      s"=${toPiece.toAn.toString}",
      toPiece.toAn.toString,
      s"(${toPiece.toAn.toString})",
      s"/${toPiece.toAn.toString}"
    )

  protected def capture(a: CaptureAction, i: PreParseInsights) =
    fromPiece(a) * captureFromPos(a) * captureDash * toPos(a, withJustX = true) * checkAndCheckmate(a) * moveQuality(i)

  protected def capturePromote(a: CapturePromoteAction, i: PreParseInsights) =
    fromPiece(a) *
      captureFromPos(a) * captureDash * toPos(a, withJustX = true) * genericPromotion(
      a.promotePiece) * checkAndCheckmate(a) * moveQuality(i)

  protected def enPassantCapture(a: EnPassantCaptureAction, i: PreParseInsights) =
    a.fromPawn.pos.toAn.x.toString * (a.fromPawn.pos + a.delta).toAn.toString * Set(
      "e.p.",
      "") * checkAndCheckmate(a) * moveQuality(i)

  protected def castling(a: CastlingAction, i: PreParseInsights): Set[String] =
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
    Set(qualifier)
  }

  override def prepareMatchString(s: String): List[(String, PreParseInsights)] = {
    def preParseInsights(s: String) = PreParseInsights(
      good = s.matches(""".*[^!?]?![^!?]?.*"""),
      excellent = s.matches(""".*!!.*"""),
      poor = s.matches(""".*[^!?]?\?[^!?]?.*"""),
      blunder = s.matches(""".*\?\?.*"""),
      maybeGood = s.matches(""".*!\?.*"""),
      maybeBad = s.matches(""".*\?!.*""")
    )
    s.toUpperCase.replaceAll(""" ch""", "ch")
      .replaceAll(""" dis[. ]{0,2}ch\.?""", "ch")
      .replaceAll(""" dblch""", "dblch")
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
