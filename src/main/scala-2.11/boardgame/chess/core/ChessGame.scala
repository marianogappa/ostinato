package boardgame.chess.core

import boardgame.core._

object ChessGame {
  def fromString(string: String, rules: ChessRules = ChessRules.default): ChessGame = {
    val (white, black) = (WhiteChessPlayer, BlackChessPlayer)
    val charVector = string.split('\n').mkString.zipWithIndex.toVector
    val grid = charVector map {
      case ('♜', pos) ⇒ Some(new Rook(Point.fromPos(pos), white))
      case ('♞', pos) ⇒ Some(new Knight(Point.fromPos(pos), white))
      case ('♝', pos) ⇒ Some(new Bishop(Point.fromPos(pos), white))
      case ('♛', pos) ⇒ Some(new Queen(Point.fromPos(pos), white))
      case ('♚', pos) ⇒ Some(new King(Point.fromPos(pos), white))
      case ('♟', pos) ⇒ Some(new Pawn(Point.fromPos(pos), white, rules.whitePawnDirection))
      case ('♖', pos) ⇒ Some(new Rook(Point.fromPos(pos), black))
      case ('♘', pos) ⇒ Some(new Knight(Point.fromPos(pos), black))
      case ('♗', pos) ⇒ Some(new Bishop(Point.fromPos(pos), black))
      case ('♕', pos) ⇒ Some(new Queen(Point.fromPos(pos), black))
      case ('♔', pos) ⇒ Some(new King(Point.fromPos(pos), black))
      case ('♙', pos) ⇒ Some(new Pawn(Point.fromPos(pos), black, rules.whitePawnDirection * -1))
      case _          ⇒ None
    }

    val enPassantPawns = charVector flatMap {
      case ('↑', pos) ⇒ EnPassantPawn.fromXYDY(Point.fromPos(pos), -1, grid)
      case ('↓', pos) ⇒ EnPassantPawn.fromXYDY(Point.fromPos(pos), 1, grid)
      case _          ⇒ None
    }

    // TODO: headOption means keep only the first; this is incorrect: if there's 2 there's a problem!
    new ChessGame(new ChessBoard(grid, enPassantPawns.headOption), List(white, black), rules)
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
  val whitePlayer = players.filter(_ == WhiteChessPlayer).head
  val blackPlayer = players.filter(_ == BlackChessPlayer).head
}

class ChessBoard(
  grid: Vector[Option[ChessPiece]],
  val enPassantPawn: Option[EnPassantPawn],
  val canCastle: Map[ChessPlayer, Boolean] = Map(WhiteChessPlayer -> true, BlackChessPlayer -> true)) extends Board[ChessPiece, ChessMovement, ChessBoard, ChessRules](grid) {

  def move(m: ChessMovement)(implicit rules: ChessRules) = {
    val resultingEnPassants = m match {
      case EnPassantMovement(pawn, delta) ⇒
        Some(EnPassantPawn(pawn.point + Point(0, math.signum(delta.y)), pawn.movedTo(pawn.point + Point(0, delta.y))))
      case _ ⇒
        None
    }

    val enPassantUpdate = m match {
      case EnPassantTakeMovement(_, _, toPawn) ⇒ List((toPawn.point.toPos, None))
      case _                                   ⇒ List()
    }

    val normalUpdates = List(
      (m.fromPiece.point.toPos, None),
      ((m.fromPiece.point + m.delta).toPos, Some(m.fromPiece.movedTo(m.fromPiece.point + m.delta)))
    )

    val updates = normalUpdates ++ enPassantUpdate
    def applyUpdate(grid: Vector[Option[ChessPiece]], update: (Int, Option[ChessPiece])) = grid.updated(update._1, update._2)

    new ChessBoard(updates.foldLeft(grid)(applyUpdate), resultingEnPassants)
  }

  def movement(from: Point, delta: Point)(implicit rules: ChessRules): Option[ChessMovement] = {
    val to = from + delta
    val fromPiece = get(from)
    val toPiece = get(to)
    lazy val betweenLocationsFree = between(from, to) forall isEmptyCell
    def isEnPassantPawn(point: Point) = enPassantPawn.exists(epp ⇒ epp.from == point)

    val validateMovement: Option[ChessMovement] = (fromPiece, toPiece, enPassantPawn) match {
      case (Some(Some(p: Pawn)), Some(None), Some(epp: EnPassantPawn)) if delta.x != 0 && isEnPassantPawn(to) && epp.pawn.owner != p.owner ⇒
        Some(EnPassantTakeMovement(p, delta, epp.pawn))

      case (Some(Some(p: Pawn)), Some(None), _) if delta.x == 0 && math.abs(delta.y) == 2 && betweenLocationsFree ⇒
        Some(EnPassantMovement(p, delta))

      case (Some(Some(p: Pawn)), Some(None), _) if delta.x == 0 && math.abs(delta.y) == 1 && to.y == Pawn.promotingPosition(delta.y) ⇒
        Some(PromoteMovement(p, delta))

      case (Some(Some(p: Pawn)), Some(None), _) if delta.x == 0 && math.abs(delta.y) == 1 ⇒
        Some(MoveMovement(p, delta))

      case (Some(Some(p: Pawn)), Some(Some(toP: ChessPiece)), _) if delta.x != 0 && (!toP.isKing || rules.kingIsTakeable) && toP.owner != p.owner ⇒
        Some(TakeMovement(p, delta, toP))

      case (Some(Some(p: ChessPiece)), Some(None), _) if !p.isPawn && betweenLocationsFree ⇒
        Some(MoveMovement(p, delta))

      case (Some(Some(p: ChessPiece)), Some(Some(toP: ChessPiece)), _) if !p.isPawn && betweenLocationsFree && (!toP.isKing || rules.kingIsTakeable) && toP.owner != p.owner ⇒
        Some(TakeMovement(p, delta, toP))

      case _ ⇒ None
    }

    def validateAfterMovement(m: ChessMovement): Option[ChessMovement] = {
      val newBoard = move(m)
      val isPlayersKingThreatened = m.fromPiece.owner.kingPiece(newBoard).map(!_.isThreatened(newBoard)).getOrElse(true)
      lazy val isCheckMate = rules.checkForCheckmates && newBoard.isLossFor(m.fromPiece.owner.enemy)

      Some(m) filter (_ ⇒ isPlayersKingThreatened) map (m ⇒ if (isCheckMate) CheckMateMovement.from(m) else m)
    }

    validateMovement flatMap validateAfterMovement
  }

  def isDrawFor(player: ChessPlayer)(implicit rules: ChessRules) = player.movements(this).isEmpty && !isLossFor(player)
  def isLossFor(player: ChessPlayer)(implicit rules: ChessRules): Boolean = {
    val noCheckForMates = rules.copy(checkForCheckmates = false)
    lazy val allNewBoards = player.movements(this)(noCheckForMates) map move
    def isKingThreatened(b: ChessBoard): Boolean = player.kingPiece(b).exists(_.isThreatened(b)(noCheckForMates))

    player.kingPiece(this).map { _.isThreatened(this)(noCheckForMates) && (allNewBoards forall isKingThreatened) } getOrElse
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
    noKingMeansLoss = false,
    checkForCheckmates = true
  )
}
case class ChessRules(
  whitePawnDirection: Int,
  kingIsTakeable: Boolean,
  allowImpossibleBoards: Boolean,
  noKingMeansLoss: Boolean,
  checkForCheckmates: Boolean) extends Rules

object Rook {
  val deltas = Piece.pointsOf(Set((-1, 0), (1, 0), (0, -1), (0, 1)))
  def char(owner: ChessPlayer) = owner match {
    case WhiteChessPlayer ⇒ '♜'
    case BlackChessPlayer ⇒ '♖'
  }
}
object Bishop {
  val deltas = Piece.pointsOf(Set((-1, -1), (1, 1), (-1, 1), (1, -1)))
  def char(owner: ChessPlayer) = owner match {
    case WhiteChessPlayer ⇒ '♝'
    case BlackChessPlayer ⇒ '♗'
  }
}
object Knight {
  val deltas = Piece.pointsOf(Set((-1, -2), (1, -2), (-1, 2), (1, 2), (-2, -1), (-2, 1), (2, -1), (2, 1)))
  def char(owner: ChessPlayer) = owner match {
    case WhiteChessPlayer ⇒ '♞'
    case BlackChessPlayer ⇒ '♘'
  }
}
object King {
  val deltas = Rook.deltas ++ Bishop.deltas
  def char(owner: ChessPlayer) = owner match {
    case WhiteChessPlayer ⇒ '♚'
    case BlackChessPlayer ⇒ '♔'
  }
}
object Queen {
  val deltas = King.deltas
  def char(owner: ChessPlayer) = owner match {
    case WhiteChessPlayer ⇒ '♛'
    case BlackChessPlayer ⇒ '♕'
  }
}
object Pawn {
  def deltas(dy: Int, isInInitialPosition: Boolean) =
    Piece.pointsOf(Set((-1, dy), (0, dy), (1, dy)) ++ (if (isInInitialPosition) Set((0, 2 * dy)) else Set()))

  def char(owner: ChessPlayer) = owner match {
    case WhiteChessPlayer ⇒ '♟'
    case BlackChessPlayer ⇒ '♙'
  }
  def promotingPosition(dy: Int)(implicit boardSize: BoardSize) = Map(-1 -> 0, 1 -> (boardSize.x - 1))(dy)
}

abstract class ChessPiece(point: Point, owner: ChessPlayer) extends Piece[ChessPlayer, ChessMovement, ChessBoard, ChessRules](point, owner) {
  val isKing = false
  val isPawn = false
  def isThreatened(board: ChessBoard)(implicit rules: ChessRules): Boolean = threatenedBy(board).nonEmpty
  def isDefended(board: ChessBoard)(implicit rules: ChessRules): Boolean = defendedBy(board).nonEmpty

  def threatenedBy(board: ChessBoard)(implicit rules: ChessRules): Set[ChessPiece] =
    otherPlayer.pieces(board).filter(_.canMoveTo(point, board)(rules.copy(kingIsTakeable = true)))

  def defendedBy(board: ChessBoard)(implicit rules: ChessRules): Set[ChessPiece] =
    owner.pieces(board).filter(_.canMoveTo(point, board.move(new ChessMovement(withOwner(otherPlayer), Point(0, 0)))))

  def canMoveTo(to: Point, board: ChessBoard)(implicit rules: ChessRules) = movements(board).exists {
    m ⇒ (point + m.delta) == to
  }

  def otherPlayer: ChessPlayer = this.owner.enemy
  def withOwner(newOwner: ChessPlayer): ChessPiece
  def equals(that: ChessPiece) = point == that.point && owner == that.owner
  override def toString = s"${owner.name}'s $pieceName on (${point.x}, ${point.y})"
  def movedTo(point: Point): ChessPiece // N.B. unsafe (doesn't check bounds)
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement]
  val toChar: Char
  val pieceName: String
}

// TODO it's easy to implement threatens: Set[ChessPiece]
// TODO override toString
class ChessMovement(val fromPiece: ChessPiece, val delta: Point) extends Movement[ChessPiece](fromPiece, delta)
case class TakeMovement(override val fromPiece: ChessPiece, override val delta: Point, toPiece: ChessPiece) extends ChessMovement(fromPiece, delta)
case class MoveMovement(override val fromPiece: ChessPiece, override val delta: Point) extends ChessMovement(fromPiece, delta)
case class EnPassantTakeMovement(fromPawn: Pawn, override val delta: Point, toPawn: Pawn) extends ChessMovement(fromPawn, delta)
case class EnPassantMovement(fromPawn: Pawn, override val delta: Point) extends ChessMovement(fromPawn, delta)
case class PromoteMovement(override val fromPiece: Pawn, override val delta: Point) extends ChessMovement(fromPiece, delta)

object CheckMateMovement {
  def from(m: ChessMovement) = CheckMateMovement(m.fromPiece, m.delta)
}
case class CheckMateMovement(override val fromPiece: ChessPiece, override val delta: Point) extends ChessMovement(fromPiece, delta)

class Rook(point: Point, owner: ChessPlayer) extends ChessPiece(point, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    Rook.deltas.flatMap { case delta ⇒ allMovementsOfDelta(point, delta, board) }
  }
  val toChar = Rook.char(owner)
  val pieceName = "Rook"
  def withOwner(newOwner: ChessPlayer) = new Rook(point, newOwner)
  def movedTo(newPoint: Point) = new Rook(newPoint, owner)
}

class Bishop(point: Point, owner: ChessPlayer) extends ChessPiece(point, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    Bishop.deltas.flatMap { case delta ⇒ allMovementsOfDelta(point, delta, board) }
  }
  val toChar = Bishop.char(owner)
  val pieceName = "Bishop"
  def withOwner(newOwner: ChessPlayer) = new Bishop(point, newOwner)
  def movedTo(newPoint: Point) = new Bishop(newPoint, owner)
}

class Knight(point: Point, owner: ChessPlayer) extends ChessPiece(point, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    Knight.deltas.flatMap { case delta ⇒ movementOfDelta(point, delta, board) }
  }
  val toChar = Knight.char(owner)
  val pieceName = "Knight"
  def withOwner(newOwner: ChessPlayer) = new Knight(point, newOwner)
  def movedTo(newPoint: Point) = new Knight(newPoint, owner)
}

class Queen(point: Point, owner: ChessPlayer) extends ChessPiece(point, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    Queen.deltas.flatMap { case delta ⇒ allMovementsOfDelta(point, delta, board) }
  }
  val toChar = Queen.char(owner)
  val pieceName = "Queen"
  def withOwner(newOwner: ChessPlayer) = new Queen(point, newOwner)
  def movedTo(newPoint: Point) = new Queen(newPoint, owner)
}

class King(point: Point, owner: ChessPlayer) extends ChessPiece(point, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    King.deltas.flatMap { case delta ⇒ movementOfDelta(point, delta, board) }
  }

  def initialY(owner: ChessPlayer)(implicit rules: ChessRules, boardSize: BoardSize) =
    if (owner == WhiteChessPlayer && rules.whitePawnDirection == 1 ||
      owner == BlackChessPlayer && rules.whitePawnDirection == -1)
      0
    else
      boardSize.x - 1

  def isInInitialPosition(implicit rules: ChessRules) = point.x == 4 && point.y == initialY(owner)
  val toChar = King.char(owner)
  val pieceName = "King"
  override val isKing = true
  def withOwner(newOwner: ChessPlayer) = new King(point, newOwner)
  def movedTo(newPoint: Point) = new King(newPoint, owner)
}
class Pawn(point: Point, owner: ChessPlayer, dy: Int) extends ChessPiece(point, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    Pawn.deltas(dy, isInInitialPosition).flatMap { case (delta) ⇒ movementOfDelta(point, delta, board) }
  }
  val isInInitialPosition = dy == 1 && point.y == 1 || dy == -1 && point.y == chessBoardSize.y - 2
  val isPromoting = point.y == Pawn.promotingPosition(dy)
  val toChar = Pawn.char(owner)
  val pieceName = "Pawn"
  override val isPawn = true
  def withOwner(newOwner: ChessPlayer) = new Pawn(point, newOwner, dy)
  def movedTo(newPoint: Point) = new Pawn(newPoint, owner, dy)
}

object EnPassantPawn {
  def posDy(point: Point, dy: Int) = (point + Point(0, dy)).toPos

  def fromXYDY(point: Point, dy: Int, grid: Vector[Option[ChessPiece]]): Option[EnPassantPawn] = {
    if (point.exists && (point + Point(0, dy)).exists) {
      grid(posDy(point, dy)) map {
        case p: Pawn ⇒ EnPassantPawn(point, p)
      }
    } else None
  }
}
case class EnPassantPawn(from: Point, pawn: Pawn)

case object WhiteChessPlayer extends ChessPlayer("White") {
  def enemy = BlackChessPlayer
}
case object BlackChessPlayer extends ChessPlayer("Black") {
  def enemy = WhiteChessPlayer
}

abstract class ChessPlayer(name: String) extends Player[ChessBoard, ChessMovement, ChessPiece, ChessPlayer](name) {
  def equals(that: Player[_, _, _, _]): Boolean = { that.name == name }
  def kingPiece(board: ChessBoard): Option[ChessPiece] = pieces(board).find(_.isKing)
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] =
    board.pieces.filter(_.owner == this).toSet.flatMap { p: ChessPiece ⇒ p.movements(board) }
  def enemy: ChessPlayer
}
