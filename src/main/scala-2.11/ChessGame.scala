object ChessGame {
  def fromString(string: String): ChessGame = {
    val (white, black) = (new Player("White"), new Player("Black"))
    val grid = string.split('\n').mkString.zipWithIndex.toVector map {
      case ('♜', pos) => Some(new Rook(ChessBoard.toX(pos), ChessBoard.toY(pos), white))
      case ('♞', pos) => Some(new Knight(ChessBoard.toX(pos), ChessBoard.toY(pos), white))
      case ('♝', pos) => Some(new Bishop(ChessBoard.toX(pos), ChessBoard.toY(pos), white))
      case ('♛', pos) => Some(new Queen(ChessBoard.toX(pos), ChessBoard.toY(pos), white))
      case ('♚', pos) => Some(new King(ChessBoard.toX(pos), ChessBoard.toY(pos), white))
      case ('♖', pos) => Some(new Rook(ChessBoard.toX(pos), ChessBoard.toY(pos), black))
      case ('♘', pos) => Some(new Knight(ChessBoard.toX(pos), ChessBoard.toY(pos), black))
      case ('♗', pos) => Some(new Bishop(ChessBoard.toX(pos), ChessBoard.toY(pos), black))
      case ('♕', pos) => Some(new Queen(ChessBoard.toX(pos), ChessBoard.toY(pos), black))
      case ('♔', pos) => Some(new King(ChessBoard.toX(pos), ChessBoard.toY(pos), black))
      case _ => None
    }
    new ChessGame(new ChessBoard(grid), List(white, black), defaultRules)
  }

  val defaultGame: ChessGame = fromString(
    """♜♞♝♛♚♝♞♜
      |........
      |........
      |........
      |........
      |........
      |........
      |♖♘♗♕♔♗♘♖
      |""".stripMargin)

  val defaultRules = new ChessRules
}

class ChessGame(val board: ChessBoard, val players: List[Player], val rules: ChessRules) extends Game(board, players, rules)


object ChessBoard {
  val xSize = 8
  def toX = Board.toX(xSize) _
  def toY = Board.toY(xSize) _
}

class ChessBoard(grid: Vector[Option[Piece]]) extends Board(grid, ChessBoard.xSize) {
  def winBoardFor: Option[Player] = None

  def move(m: Movement)(implicit rules: Rules) = {
    new ChessBoard(
      grid.updated(fromXY(m.fromPiece.x, m.fromPiece.y), None).
        updated(fromXY(m.fromPiece.x + m.dx, m.fromPiece.y + m.dy), Some(m.fromPiece))
    )
  }

  override def toString: String = {
    def cellToChar(cell: Cell): Char = cell match {
      case Some(piece) => (piece.owner.name, piece) match {
        case ("White", p: Rook) => '♜'
        case ("White", p: Knight) => '♞'
        case ("White", p: Bishop) => '♝'
        case ("White", p: Queen) => '♛'
        case ("White", p: King) => '♚'
        case ("Black", p: Rook) => '♖'
        case ("Black", p: Knight) => '♘'
        case ("Black", p: Bishop) => '♗'
        case ("Black", p: Queen) => '♕'
        case ("Black", p: King) => '♔'
      }
      case None => '.'
    }

    val linesOfCells = grid.grouped(8) map (_.toList)

    linesOfCells map (_ map cellToChar) map (_.mkString) mkString "\n"
  }
}

class ChessRules extends Rules

object Rook {
  val deltas = Set((-1, 0), (1, 0), (0, -1), (0, 1))
  val char = '♜'
}
object Bishop {
  val deltas = Set((-1, -1), (1, 1), (-1, 1), (1, -1))
  val char = '♝'
}
object Knight {
  val deltas = Set((-1, -2), (1, -2), (-1, 2), (1, 2), (-2, -1), (-2, 1), (2, -1), (2, 1))
  val char = '♞'
}
object King {
  val deltas = Rook.deltas ++ Bishop.deltas
  val char = '♚'
}
object Queen {
  val deltas = King.deltas
  val char = '♛'
}

class Rook(x: Int, y: Int, owner: Player) extends Piece(x, y, owner) {
  def movements(board: Board)(implicit rules: Rules): Set[Movement] = {
    Rook.deltas.flatMap { case (dx, dy) => allMovementsOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Rook.char
  override def toString = s"${owner.name}'s Rook on ($x, $y)"
}

class Bishop(x: Int, y: Int, owner: Player) extends Piece(x, y, owner) {
  def movements(board: Board)(implicit rules: Rules): Set[Movement] = {
    Bishop.deltas.flatMap { case (dx, dy) => allMovementsOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Bishop.char
  override def toString = s"${owner.name}'s Bishop on ($x, $y)"
}

class Knight(x: Int, y: Int, owner: Player) extends Piece(x, y, owner) {
  def movements(board: Board)(implicit rules: Rules): Set[Movement] = {
    Knight.deltas.flatMap { case (dx, dy) => movementOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Knight.char
  override def toString = s"${owner.name}'s Knight on ($x, $y)"
}

class Queen(x: Int, y: Int, owner: Player) extends Piece(x, y, owner) {
  def movements(board: Board)(implicit rules: Rules): Set[Movement] = {
    Queen.deltas.flatMap { case (dx, dy) => allMovementsOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Queen.char
  override def toString = s"${owner.name}'s Queen on ($x, $y)"
}

class King(x: Int, y: Int, owner: Player) extends Piece(x, y, owner) {
  def movements(board: Board)(implicit rules: Rules): Set[Movement] = {
    King.deltas.flatMap { case (dx, dy) => movementOfDelta(x, y, dx, dy, board) }
  }
  val toChar = King.char
  override def toString = s"${owner.name}'s King on ($x, $y)"
}