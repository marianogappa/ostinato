object ChessGame {
  def fromString(string: String): ChessGame = {
    val (white, black) = (WhiteChessPlayer, BlackChessPlayer)
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

class ChessGame(val board: ChessBoard, val players: List[ChessPlayer], val rules: ChessRules) extends Game[ChessBoard, ChessPlayer](board, players, rules) {
  def isGameOver(implicit rules: ChessRules): Boolean = isDraw || lossFor.nonEmpty
  def lossFor(implicit rules: ChessRules): Option[ChessPlayer] = players find (board.isLossFor(_) == true)
  def isDraw(implicit rules: ChessRules): Boolean = players exists board.isDrawFor
}

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

  def isDrawFor(player: ChessPlayer)(implicit rules: ChessRules) = player.movements(this).isEmpty && !isLossFor(player)
  def isLossFor(player: ChessPlayer)(implicit rules: ChessRules): Boolean = {
    lazy val allNewBoards = player.movements(this) map move
    def isKingThreatened(b: ChessBoard): Boolean = player.kingPiece(b).get.isThreatened(b) // N.B. king should exist

    player.kingPiece(this).map { _.isThreatened(this) && (allNewBoards forall isKingThreatened) } getOrElse
      rules.noKingMeansLoss
  }

  override def toString: String = {
    def cellToChar(cell: Cell): Char = cell map (_.toChar) getOrElse '.'
    val linesOfCells = grid.grouped(8) map (_.toList)

    linesOfCells map (_ map cellToChar) map (_.mkString) mkString "\n"
  }
}

case class ChessRules(
                       kingIsTakeable: Boolean = false,
                       allowImpossibleBoards: Boolean = false,
                       noKingMeansLoss: Boolean = false
                     ) extends Rules

object Rook {
  val deltas = Set((-1, 0), (1, 0), (0, -1), (0, 1))
  def char(owner: ChessPlayer) = owner match {
    case WhiteChessPlayer => '♜'
    case BlackChessPlayer => '♖'
  }
}
object Bishop {
  val deltas = Set((-1, -1), (1, 1), (-1, 1), (1, -1))
  def char(owner: ChessPlayer) = owner match {
    case WhiteChessPlayer => '♝'
    case BlackChessPlayer => '♗'
  }
}
object Knight {
  val deltas = Set((-1, -2), (1, -2), (-1, 2), (1, 2), (-2, -1), (-2, 1), (2, -1), (2, 1))
  def char(owner: ChessPlayer) = owner match {
    case WhiteChessPlayer => '♞'
    case BlackChessPlayer => '♘'
  }
}
object King {
  val deltas = Rook.deltas ++ Bishop.deltas
  def char(owner: ChessPlayer) = owner match {
    case WhiteChessPlayer => '♚'
    case BlackChessPlayer => '♔'
  }
}
object Queen {
  val deltas = King.deltas
  def char(owner: ChessPlayer) = owner match {
    case WhiteChessPlayer => '♛'
    case BlackChessPlayer => '♕'
  }
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
    m => (x + m.dx, y + m.dy) == (toX, toY)
  }

  def otherPlayer: ChessPlayer = this.owner.enemy
  def withOwner(newOwner: ChessPlayer): ChessPiece
  def equals(that: ChessPiece) = x == that.x && y == that.y && owner == that.owner
  def movedTo(x: Int, y: Int): ChessPiece // N.B. unsafe (doesn't check bounds)
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement]
  val toChar: Char
}

case class ChessMovement(fromPiece: ChessPiece, dx: Int, dy: Int) extends Movement[ChessPiece](fromPiece, dx, dy)

class Rook(x: Int, y: Int, owner: ChessPlayer) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    Rook.deltas.flatMap { case (dx, dy) => allMovementsOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Rook.char(owner)
  override def toString = s"${owner.name}'s Rook on ($x, $y)"
  def withOwner(newOwner: ChessPlayer) = new Rook(x, y, newOwner)
  def movedTo(newX: Int, newY: Int) = new Rook(newX, newY, owner)
}

class Bishop(x: Int, y: Int, owner: ChessPlayer) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    Bishop.deltas.flatMap { case (dx, dy) => allMovementsOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Bishop.char(owner)
  override def toString = s"${owner.name}'s Bishop on ($x, $y)"
  def withOwner(newOwner: ChessPlayer) = new Bishop(x, y, newOwner)
  def movedTo(newX: Int, newY: Int) = new Bishop(newX, newY, owner)
}

class Knight(x: Int, y: Int, owner: ChessPlayer) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    Knight.deltas.flatMap { case (dx, dy) => movementOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Knight.char(owner)
  override def toString = s"${owner.name}'s Knight on ($x, $y)"
  def withOwner(newOwner: ChessPlayer) = new Knight(x, y, newOwner)
  def movedTo(newX: Int, newY: Int) = new Knight(newX, newY, owner)
}

class Queen(x: Int, y: Int, owner: ChessPlayer) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    Queen.deltas.flatMap { case (dx, dy) => allMovementsOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Queen.char(owner)
  override def toString = s"${owner.name}'s Queen on ($x, $y)"
  def withOwner(newOwner: ChessPlayer) = new Queen(x, y, newOwner)
  def movedTo(newX: Int, newY: Int) = new Queen(newX, newY, owner)
}

class King(x: Int, y: Int, owner: ChessPlayer) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    King.deltas.flatMap { case (dx, dy) => movementOfDelta(x, y, dx, dy, board) }
  }
  val toChar = King.char(owner)
  override def toString = s"${owner.name}'s King on ($x, $y)"
  override val isKing = true
  def withOwner(newOwner: ChessPlayer) = new King(x, y, newOwner)
  def movedTo(newX: Int, newY: Int) = new King(newX, newY, owner)
}

case object WhiteChessPlayer extends ChessPlayer("White") {
  def enemy = BlackChessPlayer
}
case object BlackChessPlayer extends ChessPlayer("Black") {
  def enemy = WhiteChessPlayer
}

abstract class ChessPlayer(name: String) extends Player[ChessBoard, ChessMovement, ChessPiece, ChessPlayer](name) {
  def equals(that: Player[_,_,_,_]): Boolean = { that.name == name }
  def kingPiece(board: ChessBoard): Option[ChessPiece] = pieces(board).find(_.isKing)
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] =
    board.pieces.filter(_.owner == this).toSet.flatMap { p: ChessPiece => p.movements(board) }
  def enemy: ChessPlayer
}