package ostinato.chess.core

import ostinato.chess.core.NotationParser.PreParseInsights

trait NotationRules {
  val shortName: String
  val fullName: String
}

abstract class Notation[NR <: NotationRules] {
  def allPossibleRules: Set[NR]
}

trait ActionSerialiser {
  def r: NotationRules

  def serialiseAction(
      a: ChessAction, i: PreParseInsights): Set[(String, (ChessAction, NotationRules))] = {
    val s = a match {
      case a: MoveAction ⇒ move(a, i)
      case a: EnPassantAction ⇒ enPassant(a, i)
      case a: CaptureAction ⇒ capture(a, i)
      case a: EnPassantCaptureAction ⇒ enPassantCapture(a, i)
      case a: PromoteAction ⇒ promote(a, i)
      case a: CapturePromoteAction ⇒ capturePromote(a, i)
      case a: CastlingAction ⇒ castling(a, i)
      case a: LoseAction ⇒ lose(a, i)
      case a: DrawAction ⇒ draw(a, i)
      case _ ⇒ Set.empty[String]
    }

    s.map((_, (a, r)))
  }

  protected def move(a: MoveAction, i: PreParseInsights): Set[String]

  protected def enPassant(a: EnPassantAction, i: PreParseInsights): Set[String]

  protected def capture(a: CaptureAction, i: PreParseInsights): Set[String]

  protected def enPassantCapture(a: EnPassantCaptureAction, i: PreParseInsights): Set[String]

  protected def promote(a: PromoteAction, i: PreParseInsights): Set[String]

  protected def capturePromote(a: CapturePromoteAction, i: PreParseInsights): Set[String]

  protected def castling(a: CastlingAction, i: PreParseInsights): Set[String]

  protected def lose(a: LoseAction, i: PreParseInsights): Set[String]

  protected def draw(a: DrawAction, i: PreParseInsights): Set[String]

  def prepareMatchString(s: String): List[(String, PreParseInsights)] = {
    s.toUpperCase.replaceAll(""" ch""", "ch")
      .replaceAll(""" dis[. ]{0,2}ch\.?""", "ch")
      .replaceAll(""" dblch""", "dblch")
      .replaceAll(""" MATE""", "MATE")
      .replaceAll("""\s+|\d+\.|\[[^\]]*\]""", " ")
      .replaceAll(" +", " ")
      .replaceAll("""[\?!]*""", "")
      .trim
      .split(' ')
      .toList
      .map((_, PreParseInsights()))
  }

  implicit class CartesianProductableString(s: String) {
    def *(that: String) = Set(s + that)

    def *(that: Set[String]) =
      for {
        i1 ← Set(s)
        i2 ← that
      } yield i1 + i2
  }

  implicit class CartesianProductableStringSet(s: Set[String]) {
    def *(that: String) =
      for {
        i1 ← s
        i2 ← Set(that)
      } yield i1 + i2

    def *(that: Set[String]) =
      for {
        i1 ← s
        i2 ← that
      } yield i1 + i2
  }

  def genericPromotion(toPiece: ChessPiece) =
    Set(
      toPiece.toAn.toString,
      toPiece.toFigurine.toString,
      s"(${toPiece.toAn})",
      s"(${toPiece.toFigurine})",
      s"=${toPiece.toAn}",
      s"=${toPiece.toFigurine}",
      s"/${toPiece.toAn}",
      s"/${toPiece.toFigurine}"
    )
}
