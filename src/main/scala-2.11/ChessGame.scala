object ChessGame {
  def fromString(string: String, rules: ChessRules = ChessRules.default): ChessGame = {
    val (white, black) = (WhiteChessPlayer, BlackChessPlayer)
    val grid = string.split('\n').mkString.zipWithIndex.toVector map {
      case ('♜', pos) => Some(new Rook(ChessBoard.toX(pos), ChessBoard.toY(pos), white))
      case ('♞', pos) => Some(new Knight(ChessBoard.toX(pos), ChessBoard.toY(pos), white))
      case ('♝', pos) => Some(new Bishop(ChessBoard.toX(pos), ChessBoard.toY(pos), white))
      case ('♛', pos) => Some(new Queen(ChessBoard.toX(pos), ChessBoard.toY(pos), white))
      case ('♚', pos) => Some(new King(ChessBoard.toX(pos), ChessBoard.toY(pos), white))
      case ('♟', pos) => Some(new Pawn(ChessBoard.toX(pos), ChessBoard.toY(pos), white, rules.whitePawnDirection))
      case ('♖', pos) => Some(new Rook(ChessBoard.toX(pos), ChessBoard.toY(pos), black))
      case ('♘', pos) => Some(new Knight(ChessBoard.toX(pos), ChessBoard.toY(pos), black))
      case ('♗', pos) => Some(new Bishop(ChessBoard.toX(pos), ChessBoard.toY(pos), black))
      case ('♕', pos) => Some(new Queen(ChessBoard.toX(pos), ChessBoard.toY(pos), black))
      case ('♔', pos) => Some(new King(ChessBoard.toX(pos), ChessBoard.toY(pos), black))
      case ('♙', pos) => Some(new Pawn(ChessBoard.toX(pos), ChessBoard.toY(pos), black, rules.whitePawnDirection * -1))
      case _ => None
    }
    new ChessGame(new ChessBoard(grid), List(white, black), rules)
  }

  val defaultGame: ChessGame = fromString(
    """♜♞♝♛♚♝♞♜
      |♟♟♟♟♟♟♟♟
      |........
      |........
      |........
      |........
      |♙♙♙♙♙♙♙♙
      |♖♘♗♕♔♗♘♖
      |""".stripMargin)
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
    val from = get(fromX, fromY)
    val to = get(fromX + dx, fromY + dy)
    lazy val betweenLocationsFree = between(fromX, fromY, fromX + dx, fromY + dy) forall isEmptyCell

    val validateFromPiece: Option[ChessPiece] = (from, to) match {
      case (Some(Some(p: Pawn)), Some(None))
        if dx == 0 && betweenLocationsFree => Some(p)

      case (Some(Some(p: Pawn)), Some(Some(toP: ChessPiece)))
        if dx != 0 && (!toP.isKing || rules.kingIsTakeable) && toP.owner != p.owner => Some(p)

      case (Some(Some(p: ChessPiece)), Some(None))
        if !p.isPawn && betweenLocationsFree => Some(p)

      case (Some(Some(p: ChessPiece)), Some(Some(toP: ChessPiece)))
        if !p.isPawn && betweenLocationsFree && (!toP.isKing || rules.kingIsTakeable) && toP.owner != p.owner => Some(p)

      case _ => None
    }

    def validateMovement(fromPiece: ChessPiece): Option[ChessMovement] = {
      val m = new ChessMovement(fromPiece, dx, dy)
      val newBoard = move(m)

      // i.e. Don't allow the movement if the moving piece's owner's King ends up threatened
      Some(m).filter(_ => fromPiece.owner.kingPiece(newBoard).map(!_.isThreatened(newBoard)).getOrElse(true))
    }

    validateFromPiece flatMap validateMovement
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


object ChessRules {
  def default = ChessRules(
    whitePawnDirection = 1,
    kingIsTakeable = false,
    allowImpossibleBoards = false,
    noKingMeansLoss = false
  )
}
case class ChessRules(
                       whitePawnDirection: Int,
                       kingIsTakeable: Boolean,
                       allowImpossibleBoards: Boolean,
                       noKingMeansLoss: Boolean
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
object Pawn {
  def deltas(dy: Int, isInInitialPosition: Boolean) =
    Set((-1, dy), (0, dy), (1, dy)) ++ (if (isInInitialPosition) Set((0, 2 * dy)) else Set())

  def char(owner: ChessPlayer) = owner match {
    case WhiteChessPlayer => '♟'
    case BlackChessPlayer => '♙'
  }
  def promotingPosition(dy: Int) = Map(-1 -> 0, 1 -> (ChessBoard.xSize - 1))(dy)
}

abstract class ChessPiece(x: Int, y: Int, owner: ChessPlayer) extends Piece[ChessPlayer, ChessMovement, ChessBoard, ChessRules](x, y, owner) {
  val isKing = false
  val isPawn = false
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
  override def toString = s"${owner.name}'s $pieceName on ($x, $y)"
  def movedTo(x: Int, y: Int): ChessPiece // N.B. unsafe (doesn't check bounds)
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement]
  val toChar: Char
  val pieceName: String
}

case class ChessMovement(fromPiece: ChessPiece, dx: Int, dy: Int) extends Movement[ChessPiece](fromPiece, dx, dy)

class Rook(x: Int, y: Int, owner: ChessPlayer) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    Rook.deltas.flatMap { case (dx, dy) => allMovementsOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Rook.char(owner)
  val pieceName = "Rook"
  def withOwner(newOwner: ChessPlayer) = new Rook(x, y, newOwner)
  def movedTo(newX: Int, newY: Int) = new Rook(newX, newY, owner)
}

class Bishop(x: Int, y: Int, owner: ChessPlayer) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    Bishop.deltas.flatMap { case (dx, dy) => allMovementsOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Bishop.char(owner)
  val pieceName = "Bishop"
  def withOwner(newOwner: ChessPlayer) = new Bishop(x, y, newOwner)
  def movedTo(newX: Int, newY: Int) = new Bishop(newX, newY, owner)
}

class Knight(x: Int, y: Int, owner: ChessPlayer) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    Knight.deltas.flatMap { case (dx, dy) => movementOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Knight.char(owner)
  val pieceName = "Knight"
  def withOwner(newOwner: ChessPlayer) = new Knight(x, y, newOwner)
  def movedTo(newX: Int, newY: Int) = new Knight(newX, newY, owner)
}

class Queen(x: Int, y: Int, owner: ChessPlayer) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    Queen.deltas.flatMap { case (dx, dy) => allMovementsOfDelta(x, y, dx, dy, board) }
  }
  val toChar = Queen.char(owner)
  val pieceName = "Queen"
  def withOwner(newOwner: ChessPlayer) = new Queen(x, y, newOwner)
  def movedTo(newX: Int, newY: Int) = new Queen(newX, newY, owner)
}

class King(x: Int, y: Int, owner: ChessPlayer) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    King.deltas.flatMap { case (dx, dy) => movementOfDelta(x, y, dx, dy, board) }
  }
  val toChar = King.char(owner)
  val pieceName = "King"
  override val isKing = true
  def withOwner(newOwner: ChessPlayer) = new King(x, y, newOwner)
  def movedTo(newX: Int, newY: Int) = new King(newX, newY, owner)
}
class Pawn(x: Int, y: Int, owner: ChessPlayer, dy: Int) extends ChessPiece(x, y, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    Pawn.deltas(dy, isInInitialPosition).flatMap { case (mdx, mdy) => movementOfDelta(x, y, mdx, mdy, board) }
  }
  val isInInitialPosition = dy == 1 && y == 1 || dy == -1 && y == 6
  val isPromoting = y == Pawn.promotingPosition(dy)
  val toChar = Pawn.char(owner)
  val pieceName = "Pawn"
  override val isPawn = true
  def withOwner(newOwner: ChessPlayer) = new Pawn(x, y, newOwner, dy)
  def movedTo(newX: Int, newY: Int) = new Pawn(newX, newY, owner, dy)
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