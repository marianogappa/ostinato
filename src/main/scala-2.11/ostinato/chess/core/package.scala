package ostinato.chess

import ostinato.core.{ XY, BoardSize }

package object core {
  implicit val chessBoardSize = BoardSize(8, 8)

  object CastlingSide extends Enumeration {
    type CastlingSide = Value
    val Queenside, Kingside = Value
  }

  object SquareColor extends Enumeration {
    type SquareColor = Value
    val Light, Dark = Value
  }

  lazy val chessPlayers: List[ChessPlayer] = List(WhiteChessPlayer, BlackChessPlayer)
  lazy val castlingSides = List(CastlingSide.Queenside, CastlingSide.Kingside)

  lazy val castlingFullyAvailable: Map[(ChessPlayer, CastlingSide.Value), Boolean] = (for {
    chessPlayer ← chessPlayers
    castlingSide ← castlingSides
  } yield (chessPlayer, castlingSide) -> true).toMap

  lazy val castlingFullyUnavailable = castlingFullyAvailable map (kv => (kv._1, false))
  lazy val castlingOnlyBlackAvailable = castlingFullyAvailable map { case ((p, s), v) => ((p, s), p == BlackChessPlayer) }
  lazy val castlingOnlyWhiteAvailable = castlingFullyAvailable map { case ((p, s), v) => ((p, s), p == WhiteChessPlayer) }

  object ChessXY {
    lazy val chars = "abcdefgh"
    def fromAn(string: String)(implicit rules: ChessRules = ChessRules.default) = {
        val s = string.filter(_ > ' ').toLowerCase
      if (s.length == 2 && s.matches("""[a-h][1-8]"""))
        if (rules.whitePawnDirection == 1)
          Some(XY(chars.indexOf(s(0)), s(1).asDigit - 1))
        else
          Some(XY(chars.indexOf(s(0)), chessBoardSize.y - 1 - (s(1).asDigit - 1)))
      else
        None
    }
  }

  object ChessGrid {
    def fromGridString(s: String)(implicit rules: ChessRules = ChessRules.default): Vector[Option[ChessPiece]] = {
      charVector(s) map {
        case ('♜', i) ⇒ Some(♜(XY.fromI(i), BlackChessPlayer))
        case ('♞', i) ⇒ Some(♞(XY.fromI(i), BlackChessPlayer))
        case ('♝', i) ⇒ Some(♝(XY.fromI(i), BlackChessPlayer))
        case ('♛', i) ⇒ Some(♛(XY.fromI(i), BlackChessPlayer))
        case ('♚', i) ⇒ Some(♚(XY.fromI(i), BlackChessPlayer))
        case ('♟', i) ⇒ Some(♟(XY.fromI(i), BlackChessPlayer, rules.whitePawnDirection * -1))
        case ('♖', i) ⇒ Some(♜(XY.fromI(i), WhiteChessPlayer))
        case ('♘', i) ⇒ Some(♞(XY.fromI(i), WhiteChessPlayer))
        case ('♗', i) ⇒ Some(♝(XY.fromI(i), WhiteChessPlayer))
        case ('♕', i) ⇒ Some(♛(XY.fromI(i), WhiteChessPlayer))
        case ('♔', i) ⇒ Some(♚(XY.fromI(i), WhiteChessPlayer))
        case ('♙', i) ⇒ Some(♟(XY.fromI(i), WhiteChessPlayer, rules.whitePawnDirection))
        case _        ⇒ None
      }
    }
    def charVector(s: String) = s.split('\n').mkString.zipWithIndex.toVector
  }

  implicit class ChessXY(pos: XY) {
    def squareColor = if ((pos.x + pos.y) % 2 == 0) SquareColor.Light else SquareColor.Dark

    def toAn(implicit rules: ChessRules = ChessRules.default, chessBoardSize: BoardSize) = {
      if (rules.whitePawnDirection == 1)
        AnPos(ChessXY.chars(chessBoardSize.x - 1 - pos.x), pos.y + 1)
      else
        AnPos(ChessXY.chars(pos.x), chessBoardSize.y - pos.y)
    }

    lazy val dnConversions =
      Map('a' -> Set("QR", "R"), 'b' -> Set("QN", "N"), 'c' -> Set("QB", "B"), 'd' -> Set("Q"), 'e' -> Set("K"),
        'f' -> Set("KB", "B"), 'g' -> Set("KN", "N"), 'h' -> Set("KR", "R"))

    lazy val iccfConversions =
      Map('a' -> 1, 'b' -> 2, 'c' -> 3, 'd' -> 4, 'e' -> 5, 'f' -> 6, 'g' -> 7, 'h' -> 8)

    def toDn(turn: ChessPlayer)(implicit rules: ChessRules = ChessRules.default, chessBoardSize: BoardSize) = {
      (toAn, turn) match {
        case (AnPos(x, y), WhiteChessPlayer) => dnConversions(x) map (_ + y)
        case (AnPos(x, y), BlackChessPlayer) => dnConversions(x) map (_ + (9 - y))
      }
    }

    def toIccf(implicit rules: ChessRules = ChessRules.default, chessBoardSize: BoardSize) = {
      val an = toAn
      IccfPos(iccfConversions(an.x), an.y)
    }
  }

  implicit class ChessAnAction(action: ChessAction) {
    def allPossibleNotations(implicit rules: ChessRules = ChessRules.default): Set[String] =
      Notation.allPossibleNotations(action)
  }

  case class AnPos(x: Char, y: Int) {
    override def toString = s"$x$y"
  }

  case class IccfPos(x: Int, y: Int) {
    override def toString = s"$x$y"
  }

  object Fan {
    def checkmate(winner: ChessPlayer) = if (winner == WhiteChessPlayer) "1-0" else "0-1"
    def check(implicit rules: ChessRules = ChessRules.default) = "+"
    def kingSideCastle(implicit rules: ChessRules = ChessRules.default) = "0-0"
    def queenSideCastle(implicit rules: ChessRules = ChessRules.default) = "0-0-0"
    def draw(implicit rules: ChessRules = ChessRules.default) = "½–½"
  }

  case class PastBoards(boards: Map[String, Int] = Map()) {
    def serialiseBoard(board: ChessBoard): String = "^([^ ]+ +[^ ]+ +[^ ]+ +[^ ]+).*$".r.replaceFirstIn(board.toFen, "$1")
    lazy val isInThreefoldRepetition = boards.values.exists(_ >= 3)
    val isEmpty = boards.isEmpty

    def withBoard(board: ChessBoard) = {
      val serialisedBoard = serialiseBoard(board)
      if (boards.get(serialisedBoard).nonEmpty)
        PastBoards(boards.updated(serialisedBoard, boards(serialisedBoard) + 1))
      else
        PastBoards(boards ++ Map(serialisedBoard -> 1))
    }
  }
}
