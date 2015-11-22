package boardgame.chess.core

import boardgame.core._

object ChessGame {
  def fromString(string: String, rules: ChessRules = ChessRules.default): ChessGame = {
    val (white, black) = (WhiteChessPlayer, BlackChessPlayer)
    val charVector = string.split('\n').mkString.zipWithIndex.toVector
    val grid = charVector map {
      case ('♜', i) ⇒ Some(new ♜(XY.fromI(i), white))
      case ('♞', i) ⇒ Some(new ♞(XY.fromI(i), white))
      case ('♝', i) ⇒ Some(new ♝(XY.fromI(i), white))
      case ('♛', i) ⇒ Some(new ♛(XY.fromI(i), white))
      case ('♚', i) ⇒ Some(new ♚(XY.fromI(i), white))
      case ('♟', i) ⇒ Some(new ♟(XY.fromI(i), white, rules.whitePawnDirection))
      case ('♖', i) ⇒ Some(new ♜(XY.fromI(i), black))
      case ('♘', i) ⇒ Some(new ♞(XY.fromI(i), black))
      case ('♗', i) ⇒ Some(new ♝(XY.fromI(i), black))
      case ('♕', i) ⇒ Some(new ♛(XY.fromI(i), black))
      case ('♔', i) ⇒ Some(new ♚(XY.fromI(i), black))
      case ('♙', i) ⇒ Some(new ♟(XY.fromI(i), black, rules.whitePawnDirection * -1))
      case _        ⇒ None
    }

    val enPassantPawns = charVector flatMap {
      case ('↑', i) ⇒ EnPassantPawn.fromXYD(XY.fromI(i), XY(0, -1), grid)
      case ('↓', i) ⇒ EnPassantPawn.fromXYD(XY.fromI(i), XY(0, 1), grid)
      case _        ⇒ None
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
        Some(EnPassantPawn(pawn.pos + XY(0, math.signum(delta.y)), pawn.movedTo(pawn.pos + XY(0, delta.y))))
      case _ ⇒
        None
    }

    val specialUpdates = m match {
      case EnPassantTakeMovement(_, _, toPawn) ⇒
        List((toPawn.pos.toI, None))

      case CastlingMovement(_, _, rook, rookDelta) ⇒
        List((rook.pos.toI, None), ((rook.pos + rookDelta).toI, Some(rook.movedTo(rook.pos + rookDelta))))

      case _ ⇒ List()
    }

    val normalUpdates = List(
      (m.fromPiece.pos.toI, None),
      ((m.fromPiece.pos + m.delta).toI, Some(m.fromPiece.movedTo(m.fromPiece.pos + m.delta)))
    )

    val updates = normalUpdates ++ specialUpdates
    def applyUpdate(grid: Vector[Option[ChessPiece]], update: (Int, Option[ChessPiece])) = grid.updated(update._1, update._2)

    new ChessBoard(updates.foldLeft(grid)(applyUpdate), resultingEnPassants)
  }

  def movement(from: XY, delta: XY)(implicit rules: ChessRules): Option[ChessMovement] = {
    val to = from + delta
    val fromPiece = get(from)
    val toPiece = get(to)
    lazy val betweenLocationsFree = between(from, to) forall isEmptyCell
    def betweenLocationsNotThreatenedBy(player: ChessPlayer) =
      xyBetween(from, to) forall (pos ⇒ player.pieces(this) forall (!_.canMoveTo(pos, this)))

    def isEnPassantPawn(pos: XY) = enPassantPawn.exists(epp ⇒ epp.from == pos)

    def targetRook(k: ♚) = get(k.targetRookPosition(delta.x)) match {
      case Some(Some(r: ♜)) if r.owner == k.owner ⇒ Some(r)
      case _                                         ⇒ None
    }

    val validateMovement: Option[ChessMovement] = (fromPiece, toPiece, enPassantPawn) match {
      case (Some(Some(p: ♟)), Some(None), Some(epp: EnPassantPawn)) if delta.x != 0 && isEnPassantPawn(to) && epp.pawn.owner != p.owner ⇒
        Some(EnPassantTakeMovement(p, delta, epp.pawn))

      case (Some(Some(p: ♟)), Some(None), _) if delta.x == 0 && math.abs(delta.y) == 2 && betweenLocationsFree ⇒
        Some(EnPassantMovement(p, delta))

      case (Some(Some(p: ♟)), Some(None), _) if delta.x == 0 && math.abs(delta.y) == 1 && to.y == ♟.promotingPosition(delta.y) ⇒
        Some(PromoteMovement(p, delta))

      case (Some(Some(p: ♟)), Some(None), _) if delta.x == 0 && math.abs(delta.y) == 1 ⇒
        Some(MoveMovement(p, delta))

      case (Some(Some(p: ♟)), Some(Some(toP: ChessPiece)), _) if delta.x != 0 && (!toP.isKing || rules.kingIsTakeable) && toP.owner != p.owner ⇒
        Some(TakeMovement(p, delta, toP))

      case (Some(Some(k: ♚)), _, _) if math.abs(delta.x) == 2 ⇒
        (toPiece, targetRook(k)) match {
          case (Some(None), Some(r: ♜)) if k.isInInitialPosition && canCastle(k.owner) && !k.isThreatened(this) &&
            betweenLocationsFree && betweenLocationsNotThreatenedBy(k.owner.enemy) ⇒

            Some(CastlingMovement(k, delta, r, ♚.rookDeltaFor(delta)))

          case _ ⇒ None
        }

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

object ♜ {
  val deltas = Piece.toXYs(Set((-1, 0), (1, 0), (0, -1), (0, 1)))
  def char(owner: ChessPlayer) = owner match {
    case WhiteChessPlayer ⇒ '♜'
    case BlackChessPlayer ⇒ '♖'
  }
}
object ♝ {
  val deltas = Piece.toXYs(Set((-1, -1), (1, 1), (-1, 1), (1, -1)))
  def char(owner: ChessPlayer) = owner match {
    case WhiteChessPlayer ⇒ '♝'
    case BlackChessPlayer ⇒ '♗'
  }
}
object ♞ {
  val deltas = Piece.toXYs(Set((-1, -2), (1, -2), (-1, 2), (1, 2), (-2, -1), (-2, 1), (2, -1), (2, 1)))
  def char(owner: ChessPlayer) = owner match {
    case WhiteChessPlayer ⇒ '♞'
    case BlackChessPlayer ⇒ '♘'
  }
}
object ♚ {
  def deltas(addCastlingDeltas: Boolean) = normalDeltas ++ (if (addCastlingDeltas) Piece.toXYs(Set((-2, 0), (2, 0))) else Set())
  def normalDeltas = ♜.deltas ++ ♝.deltas
  def rookDeltaFor(kingDelta: XY) = XY(if (kingDelta.x < 0) 3 else -2, 0)
  def char(owner: ChessPlayer) = owner match {
    case WhiteChessPlayer ⇒ '♚'
    case BlackChessPlayer ⇒ '♔'
  }
}
object ♛ {
  val deltas = ♚.normalDeltas
  def char(owner: ChessPlayer) = owner match {
    case WhiteChessPlayer ⇒ '♛'
    case BlackChessPlayer ⇒ '♕'
  }
}
object ♟ {
  def deltas(dy: Int, isInInitialPosition: Boolean) =
    Piece.toXYs(Set((-1, dy), (0, dy), (1, dy)) ++ (if (isInInitialPosition) Set((0, 2 * dy)) else Set()))

  def char(owner: ChessPlayer) = owner match {
    case WhiteChessPlayer ⇒ '♟'
    case BlackChessPlayer ⇒ '♙'
  }
  def promotingPosition(dy: Int)(implicit boardSize: BoardSize) = Map(-1 -> 0, 1 -> (boardSize.x - 1))(dy)
}

abstract class ChessPiece(pos: XY, owner: ChessPlayer) extends Piece[ChessPlayer, ChessMovement, ChessBoard, ChessRules](pos, owner) {
  val isKing = false
  val isPawn = false
  val isRook = false
  def isThreatened(board: ChessBoard)(implicit rules: ChessRules): Boolean = threatenedBy(board).nonEmpty
  def isDefended(board: ChessBoard)(implicit rules: ChessRules): Boolean = defendedBy(board).nonEmpty

  def threatenedBy(board: ChessBoard)(implicit rules: ChessRules): Set[ChessPiece] =
    otherPlayer.pieces(board).filter(_.canMoveTo(pos, board)(rules.copy(kingIsTakeable = true)))

  def defendedBy(board: ChessBoard)(implicit rules: ChessRules): Set[ChessPiece] =
    owner.pieces(board).filter(_.canMoveTo(pos, board.move(new ChessMovement(withOwner(otherPlayer), XY(0, 0)))))

  def canMoveTo(to: XY, board: ChessBoard)(implicit rules: ChessRules) = movements(board).exists {
    m ⇒ (pos + m.delta) == to
  }

  def otherPlayer: ChessPlayer = this.owner.enemy
  def withOwner(newOwner: ChessPlayer): ChessPiece
  def equals(that: ChessPiece) = pos == that.pos && owner == that.owner
  override def toString = s"${owner.name}'s $pieceName on (${pos.x}, ${pos.y})"
  def movedTo(pos: XY): ChessPiece // N.B. unsafe (doesn't check bounds)
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement]
  val toChar: Char
  val pieceName: String
}

// TODO it's easy to implement threatens: Set[ChessPiece]
// TODO override toString
class ChessMovement(val fromPiece: ChessPiece, val delta: XY) extends Movement[ChessPiece](fromPiece, delta)
case class TakeMovement(override val fromPiece: ChessPiece, override val delta: XY, toPiece: ChessPiece) extends ChessMovement(fromPiece, delta)
case class MoveMovement(override val fromPiece: ChessPiece, override val delta: XY) extends ChessMovement(fromPiece, delta)
case class EnPassantTakeMovement(fromPawn: ♟, override val delta: XY, toPawn: ♟) extends ChessMovement(fromPawn, delta)
case class EnPassantMovement(fromPawn: ♟, override val delta: XY) extends ChessMovement(fromPawn, delta)
case class PromoteMovement(override val fromPiece: ♟, override val delta: XY) extends ChessMovement(fromPiece, delta)
case class CastlingMovement(override val fromPiece: ♚, kingDelta: XY, targetRook: ♜, rookDelta: XY) extends ChessMovement(fromPiece, kingDelta)

object CheckMateMovement {
  def from(m: ChessMovement) = CheckMateMovement(m.fromPiece, m.delta)
}
case class CheckMateMovement(override val fromPiece: ChessPiece, override val delta: XY) extends ChessMovement(fromPiece, delta)

class ♜(pos: XY, owner: ChessPlayer) extends ChessPiece(pos, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    ♜.deltas.flatMap { case delta ⇒ allMovementsOfDelta(pos, delta, board) }
  }
  val toChar = ♜.char(owner)
  val pieceName = "Rook"
  override val isRook = true
  def withOwner(newOwner: ChessPlayer) = new ♜(pos, newOwner)
  def movedTo(newXY: XY) = new ♜(newXY, owner)
}

class ♝(pos: XY, owner: ChessPlayer) extends ChessPiece(pos, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    ♝.deltas.flatMap { case delta ⇒ allMovementsOfDelta(pos, delta, board) }
  }
  val toChar = ♝.char(owner)
  val pieceName = "Bishop"
  def withOwner(newOwner: ChessPlayer) = new ♝(pos, newOwner)
  def movedTo(newXY: XY) = new ♝(newXY, owner)
}

class ♞(pos: XY, owner: ChessPlayer) extends ChessPiece(pos, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    ♞.deltas.flatMap { case delta ⇒ movementOfDelta(pos, delta, board) }
  }
  val toChar = ♞.char(owner)
  val pieceName = "Knight"
  def withOwner(newOwner: ChessPlayer) = new ♞(pos, newOwner)
  def movedTo(newXY: XY) = new ♞(newXY, owner)
}

class ♛(pos: XY, owner: ChessPlayer) extends ChessPiece(pos, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    ♛.deltas.flatMap { case delta ⇒ allMovementsOfDelta(pos, delta, board) }
  }
  val toChar = ♛.char(owner)
  val pieceName = "Queen"
  def withOwner(newOwner: ChessPlayer) = new ♛(pos, newOwner)
  def movedTo(newXY: XY) = new ♛(newXY, owner)
}

class ♚(pos: XY, owner: ChessPlayer) extends ChessPiece(pos, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    ♚.deltas(isInInitialPosition).flatMap { case delta ⇒ movementOfDelta(pos, delta, board) }
  }

  def initialY(implicit rules: ChessRules, chessBoardSize: BoardSize) =
    if (owner == WhiteChessPlayer && rules.whitePawnDirection == 1 ||
      owner == BlackChessPlayer && rules.whitePawnDirection == -1)
      0
    else
      chessBoardSize.y - 1

  def targetRookPosition(dx: Int)(implicit rules: ChessRules) = XY(if (dx < 0) 0 else chessBoardSize.x - 1, initialY)

  def isInInitialPosition(implicit rules: ChessRules) = pos.x == 4 && pos.y == initialY
  val toChar = ♚.char(owner)
  val pieceName = "King"
  override val isKing = true
  def withOwner(newOwner: ChessPlayer) = new ♚(pos, newOwner)
  def movedTo(newXY: XY) = new ♚(newXY, owner)
}
class ♟(pos: XY, owner: ChessPlayer, dy: Int) extends ChessPiece(pos, owner) {
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] = {
    ♟.deltas(dy, isInInitialPosition).flatMap { case delta ⇒ movementOfDelta(pos, delta, board) }
  }
  val isInInitialPosition = dy == 1 && pos.y == 1 || dy == -1 && pos.y == chessBoardSize.y - 2
  val isPromoting = pos.y == ♟.promotingPosition(dy)
  val toChar = ♟.char(owner)
  val pieceName = "Pawn"
  override val isPawn = true
  def withOwner(newOwner: ChessPlayer) = new ♟(pos, newOwner, dy)
  def movedTo(newXY: XY) = new ♟(newXY, owner, dy)
}

object EnPassantPawn {
  def fromXYD(pos: XY, delta: XY, grid: Vector[Option[ChessPiece]]): Option[EnPassantPawn] = {
    if (pos.exists && (pos + delta).exists) {
      grid((pos + delta).toI) map {
        case p: ♟ ⇒ EnPassantPawn(pos, p)
      }
    } else None
  }
}
case class EnPassantPawn(from: XY, pawn: ♟)

case object WhiteChessPlayer extends ChessPlayer("White") {
  def enemy = BlackChessPlayer
}
case object BlackChessPlayer extends ChessPlayer("Black") {
  def enemy = WhiteChessPlayer
}

abstract class ChessPlayer(name: String) extends Player[ChessBoard, ChessMovement, ChessPiece, ChessPlayer](name) {
  def kingPiece(board: ChessBoard): Option[ChessPiece] = pieces(board).find(_.isKing)
  def enemy: ChessPlayer
  def movements(board: ChessBoard)(implicit rules: ChessRules): Set[ChessMovement] =
    pieces(board) flatMap (_.movements(board))
}
