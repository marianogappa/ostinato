package ostinato.chess.core

trait NotationRules {
  val shortName: String
  val fullName: String
}

abstract class Notation[NR <: NotationRules] {
  def allPossibleRules: Set[NR]
}

trait ActionParser {
  def r: NotationRules

  def parseAction(a: ChessAction)(
    implicit rules: ChessOptimisations = ChessOptimisations.default): Set[(String, (ChessAction, NotationRules))] = {
    val s = a match {
      case a: MoveAction             ⇒ move(a)
      case a: EnPassantAction        ⇒ enPassant(a)
      case a: CaptureAction          ⇒ capture(a)
      case a: EnPassantCaptureAction ⇒ enPassantCapture(a)
      case a: CapturePromoteAction   ⇒ capturePromote(a)
      case a: CastlingAction         ⇒ castling(a)
      case a: LoseAction             ⇒ lose(a)
      case a: DrawAction             ⇒ draw(a)
      case _                         ⇒ Set.empty[String]
    }

    s.map((_, (a, r)))
  }

  protected def move(a: MoveAction): Set[String]
  protected def enPassant(a: EnPassantAction): Set[String]
  protected def capture(a: CaptureAction): Set[String]
  protected def enPassantCapture(a: EnPassantCaptureAction): Set[String]
  protected def capturePromote(a: CapturePromoteAction): Set[String]
  protected def castling(a: CastlingAction)(implicit rules: ChessOptimisations = ChessOptimisations.default): Set[String]
  protected def lose(a: LoseAction): Set[String]
  protected def draw(a: DrawAction): Set[String]

  implicit class CartesianProductableString(s: String) {
    def *(that: String) = Set(s + that)
    def *(that: Set[String]) = for {
      i1 ← Set(s)
      i2 ← that
    } yield i1 + i2
  }
  implicit class CartesianProductableStringSet(s: Set[String]) {
    def *(that: String) = for {
      i1 ← s
      i2 ← Set(that)
    } yield i1 + i2
    def *(that: Set[String]) = for {
      i1 ← s
      i2 ← that
    } yield i1 + i2
  }

  def genericPromotion(toPiece: ChessPiece) =
    Set(toPiece.toAn.toString, toPiece.toFigurine.toString, s"(${toPiece.toAn})", s"(${toPiece.toFigurine})",
      s"=${toPiece.toAn}", s"=${toPiece.toFigurine}", s"/${toPiece.toAn}", s"/${toPiece.toFigurine}")
}
