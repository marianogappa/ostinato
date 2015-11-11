object ChessGame {
  def fromString(string: String): ChessGame = {
    val (white, black) = (new ChessPlayer("White"), new ChessPlayer("Black"))
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

class ChessGame(val board: ChessBoard, val players: List[ChessPlayer], val rules: ChessRules) extends Game[ChessBoard, ChessPlayer](board, players, rules)

object ChessBoard {
  val xSize = 8
  def toX = Board.toX(xSize) _
  def toY = Board.toY(xSize) _
}

class ChessBoard(grid: Vector[Option[ChessPiece]]) extends Board[ChessPiece, ChessMovement, ChessBoard, ChessRules](grid, ChessBoard.xSize) {

  def move(m: ChessMovement)(implicit rules: ChessRules) = {
    new ChessBoard(
      grid.updated(fromXY(m.fromPiece.x, m.fromPiece.y), None).
        updated(fromXY(m.fromPiece.x + m.dx, m.fromPiece.y + m.dy),
          Some(m.fromPiece.movedTo(m.fromPiece.x + m.dx,m.fromPiece.y + m.dy)))
    )
  }

  def movement(fromX: Int, fromY: Int, dx: Int, dy: Int)(implicit rules: ChessRules): Option[ChessMovement] = {
    val fromLocation = get(fromX, fromY)
    val toLocation = get(fromX + dx, fromY + dy)
    val betweenLocations = between(fromX, fromY, fromX + dx, fromY + dy)

    if  (
          isPiece(fromLocation) && (betweenLocations forall isEmptyCell) &&
          (
            isEmptyCell(toLocation) ||
            isPiece(toLocation) && (!toLocation.get.get.isKing || rules.kingIsTakeable) &&
            toLocation.get.get.owner != fromLocation.get.get.owner
          )
        )
    {
      val m = new ChessMovement(fromLocation.get.get, dx, dy)

      if (!fromLocation.get.get.isKing) {
        Some(m)
      } else {
        val boardWithMovedKing = move(m)
        if (boardWithMovedKing.get(fromX + dx, fromY + dy).get.get.isThreatened(boardWithMovedKing)) {
          None
        } else {
          Some(m)
        }
      }
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

case class ChessRules(kingIsTakeable: Boolean = false) extends Rules

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

abstract class ChessPiece(x: Int, y: Int, owner: ChessPlayer) extends Piece[ChessPlayer, ChessMovement, ChessBoard, ChessRules](x, y, owner) {
  val isKing = false
  def isThreatened(board: ChessBoard)(implicit rules: ChessRules): Boolean = threatenedBy(board).nonEmpty
  def isDefended(board: ChessBoard)(implicit rules: ChessRules): Boolean = defendedBy(board).nonEmpty

  def threatenedBy(board: ChessBoard)(implicit rules: ChessRules): Set[ChessPiece] =
    otherPlayer.pieces(board).filter(_.canMoveTo(x, y, board)(rules.copy(kingIsTakeable = true)))

  def defendedBy(board: ChessBoard)(implicit rules: ChessRules): Set[ChessPiece] =
    owner.pieces(board).filter(_.canMoveTo(x, y, board.move(new ChessMovement(withOwner(otherPlayer), 0, 0))))

  def canMoveTo(toX: Int, toY: Int, board: ChessBoard)(implicit rules: ChessRules) = movements(board).exists {
    m => (x + m.dx, y + m.dy) == (toX, toY) // TODO THIS DOESN'T WORK FOR THREATENED BY!!!
  }

  def otherPlayer: ChessPlayer =
    new ChessPlayer(List("White", "Black").filter(_ != owner.name).head)

  def withOwner(newOwner: ChessPlayer): ChessPiece

  def equals(that: ChessPiece) = x == that.x && y == that.y && owner == that.owner
  def movedTo(x: Int, y: Int): ChessPiece // N.B. unsafe (doesn't check bounds)
}

case class ChessMovement(fromPiece: ChessPiece, dx: Int, dy: Int) extends Movement[ChessPiece](fromPiece, dx, dy)

class Rook(x: Int, y: Int, owner: ChessPlayer) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    Rook.deltas.flatMap { case (dx, dy) => allMovementsOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Rook.char
  override def toString = s"${owner.name}'s Rook on ($x, $y)"
  def withOwner(newOwner: ChessPlayer) = new Rook(x, y, newOwner)
  def movedTo(newX: Int, newY: Int) = new Rook(newX, newY, owner)
}

class Bishop(x: Int, y: Int, owner: ChessPlayer) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    Bishop.deltas.flatMap { case (dx, dy) => allMovementsOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Bishop.char
  override def toString = s"${owner.name}'s Bishop on ($x, $y)"
  def withOwner(newOwner: ChessPlayer) = new Bishop(x, y, newOwner)
  def movedTo(newX: Int, newY: Int) = new Bishop(newX, newY, owner)
}

class Knight(x: Int, y: Int, owner: ChessPlayer) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    Knight.deltas.flatMap { case (dx, dy) => movementOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Knight.char
  override def toString = s"${owner.name}'s Knight on ($x, $y)"
  def withOwner(newOwner: ChessPlayer) = new Knight(x, y, newOwner)
  def movedTo(newX: Int, newY: Int) = new Knight(newX, newY, owner)
}

class Queen(x: Int, y: Int, owner: ChessPlayer) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    Queen.deltas.flatMap { case (dx, dy) => allMovementsOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Queen.char
  override def toString = s"${owner.name}'s Queen on ($x, $y)"
  def withOwner(newOwner: ChessPlayer) = new Queen(x, y, newOwner)
  def movedTo(newX: Int, newY: Int) = new Queen(newX, newY, owner)
}

class King(x: Int, y: Int, owner: ChessPlayer) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    King.deltas.flatMap { case (dx, dy) => movementOfDelta(x, y, dx, dy, board) }
  }
  val toChar = King.char
  override def toString = s"${owner.name}'s King on ($x, $y)"
  override val isKing = true
  def withOwner(newOwner: ChessPlayer) = new King(x, y, newOwner)
  def movedTo(newX: Int, newY: Int) = new King(newX, newY, owner)
}

class ChessPlayer(name: String) extends Player[ChessBoard, ChessMovement, ChessPiece, ChessPlayer](name) {
  def equals(that: Player[_,_,_,_]): Boolean = { that.name == name }
}