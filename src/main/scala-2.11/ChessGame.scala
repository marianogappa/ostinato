object ChessGame {
  def fromString(string: String): ChessGame = {
    val (white, black) = (new Player[ChessBoard, ChessMovement]("White"), new Player[ChessBoard, ChessMovement]("Black"))
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

class ChessGame(val board: ChessBoard, val players: List[Player[ChessBoard, ChessMovement]], val rules: ChessRules) extends Game[ChessBoard, Player[ChessBoard, ChessMovement]](board, players, rules)
//class Player(name: String) extends Player[ChessBoard, ChessMovement](name)

object ChessBoard {
  val xSize = 8
  def toX = Board.toX(xSize) _
  def toY = Board.toY(xSize) _
}

class ChessBoard(grid: Vector[Option[ChessPiece]]) extends Board[ChessPiece, ChessMovement, ChessBoard](grid, ChessBoard.xSize) {

  def move(m: ChessMovement)(implicit rules: Rules) = {
    new ChessBoard(
      grid.updated(fromXY(m.fromPiece.x, m.fromPiece.y), None).
        updated(fromXY(m.fromPiece.x + m.dx, m.fromPiece.y + m.dy), Some(m.fromPiece))
    )
  }

  def movement(fromX: Int, fromY: Int, dx: Int, dy: Int)(implicit rules: Rules): Option[ChessMovement] = {
    val fromLocation = get(fromX, fromY)
    val toLocation = get(fromX + dx, fromY + dy)
    val betweenLocations = between(fromX, fromY, fromX + dx, fromY + dy)

    if (isPiece(fromLocation) && (betweenLocations forall isEmptyCell) &&
      (isEmptyCell(toLocation) || isPiece(toLocation) && toLocation.get.get.owner != fromLocation.get.get.owner)) {
      Some(new ChessMovement(fromLocation.get.get, dx, dy))
    } else {
      None
    }
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

abstract class ChessPiece(x: Int, y: Int, owner: Player[ChessBoard, ChessMovement]) extends Piece[Player[ChessBoard, ChessMovement], ChessMovement, ChessBoard](x, y, owner)

class ChessMovement(override val fromPiece: ChessPiece, dx: Int, dy: Int) extends Movement[ChessPiece](fromPiece, dx, dy)

class Rook(x: Int, y: Int, owner: Player[ChessBoard, ChessMovement]) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: Rules): Set[ChessMovement] = {
    Rook.deltas.flatMap { case (dx, dy) => allMovementsOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Rook.char
  override def toString = s"${owner.name}'s Rook on ($x, $y)"
}

class Bishop(x: Int, y: Int, owner: Player[ChessBoard, ChessMovement]) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: Rules): Set[ChessMovement] = {
    Bishop.deltas.flatMap { case (dx, dy) => allMovementsOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Bishop.char
  override def toString = s"${owner.name}'s Bishop on ($x, $y)"
}

class Knight(x: Int, y: Int, owner: Player[ChessBoard, ChessMovement]) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: Rules): Set[ChessMovement] = {
    Knight.deltas.flatMap { case (dx, dy) => movementOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Knight.char
  override def toString = s"${owner.name}'s Knight on ($x, $y)"
}

class Queen(x: Int, y: Int, owner: Player[ChessBoard, ChessMovement]) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: Rules): Set[ChessMovement] = {
    Queen.deltas.flatMap { case (dx, dy) => allMovementsOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Queen.char
  override def toString = s"${owner.name}'s Queen on ($x, $y)"
}

class King(x: Int, y: Int, owner: Player[ChessBoard, ChessMovement]) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: Rules): Set[ChessMovement] = {
    King.deltas.flatMap { case (dx, dy) => movementOfDelta(x, y, dx, dy, board) }
  }
  val toChar = King.char
  override def toString = s"${owner.name}'s King on ($x, $y)"
}