package boardgame.chess.core

import boardgame.core.{ XY, Board }

class ChessBoard(
    grid: Vector[Option[ChessPiece]],
    val enPassantPawn: Option[EnPassantPawn],
    val canCastle: Map[ChessPlayer, Boolean] = Map(WhiteChessPlayer -> true, BlackChessPlayer -> true)) extends Board[ChessPiece, ChessMovement, ChessBoard, ChessRules](grid) {

  def move(m: ChessMovement)(implicit rules: ChessRules) = {
    val resultingEnPassants = m match {
      case EnPassantMovement(pawn, delta, _, _) ⇒
        Some(EnPassantPawn(pawn.pos + XY(0, math.signum(delta.y)), pawn.movedTo(pawn.pos + XY(0, delta.y))))
      case _ ⇒
        None
    }

    new ChessBoard(m.gridUpdates.foldLeft(grid)(applyUpdate), resultingEnPassants)
  }

  def movement(from: XY, delta: XY)(implicit rules: ChessRules): Set[ChessMovement] = {
    val to = from + delta
    val fromPiece = get(from)
    val toPiece = get(to)
    lazy val betweenLocationsFree = between(from, to) forall isEmptyCell
    def betweenLocationsNotThreatenedBy(player: ChessPlayer) =
      xyBetween(from, to) forall (pos ⇒ player.pieces(this) forall (!_.canMoveTo(pos, this)))

    def isEnPassantPawn(pos: XY) = enPassantPawn.exists(epp ⇒ epp.from == pos)

    def targetRook(k: ♚) = get(k.targetRookPosition(delta.x)) match {
      case Some(Some(r: ♜)) if r.owner == k.owner ⇒ Some(r)
      case _                                      ⇒ None
    }

    val validateMovement: Set[ChessMovementFactory] = (fromPiece, toPiece, enPassantPawn) match {
      case (Some(Some(p: ♟)), Some(None), Some(epp: EnPassantPawn)) if delta.x != 0 && isEnPassantPawn(to) && epp.pawn.owner != p.owner ⇒
        Set(EnPassantTakeMovementFactory(p, delta, epp.pawn))

      case (Some(Some(p: ♟)), Some(None), _) if delta.x == 0 && math.abs(delta.y) == 2 && betweenLocationsFree ⇒
        Set(EnPassantMovementFactory(p, delta))

      case (Some(Some(p: ♟)), Some(None), _) if delta.x == 0 && math.abs(delta.y) == 1 && to.y == ♟.promotingPosition(delta.y) ⇒
        Set(
          ♜(from + delta, p.owner), ♝(from + delta, p.owner),
          ♞(from + delta, p.owner), ♛(from + delta, p.owner)) map (PromoteMovementFactory(p, delta, _))

      case (Some(Some(p: ♟)), Some(None), _) if delta.x == 0 && math.abs(delta.y) == 1 ⇒
        Set(MoveMovementFactory(p, delta))

      case (Some(Some(p: ♟)), Some(Some(toP: ChessPiece)), _) if delta.x != 0 && (!toP.isKing || rules.kingIsTakeable) && toP.owner != p.owner ⇒
        Set(TakeMovementFactory(p, delta, toP))

      case (Some(Some(k: ♚)), _, _) if math.abs(delta.x) == 2 ⇒
        (toPiece, targetRook(k)) match {
          case (Some(None), Some(r: ♜)) if k.isInInitialPosition && canCastle(k.owner) && !k.isThreatened(this) &&
            betweenLocationsFree && betweenLocationsNotThreatenedBy(k.enemy) ⇒

            Set(CastlingMovementFactory(k, delta, r, ♚.rookDeltaFor(delta)))

          case _ ⇒ Set()
        }

      case (Some(Some(p: ChessPiece)), Some(None), _) if !p.isPawn && betweenLocationsFree ⇒
        Set(MoveMovementFactory(p, delta))

      case (Some(Some(p: ChessPiece)), Some(Some(toP: ChessPiece)), _) if !p.isPawn && betweenLocationsFree && (!toP.isKing || rules.kingIsTakeable) && toP.owner != p.owner ⇒
        Set(TakeMovementFactory(p, delta, toP))

      case _ ⇒ Set()
    }

    def validateAfterMovement(mf: ChessMovementFactory): Set[ChessMovement] = {
      val m = mf.complete()
      val newBoard = move(m)
      val isPlayersKingThreatened = m.fromPiece.owner.kingPiece(newBoard).map(!_.isThreatened(newBoard)).getOrElse(true)
      lazy val isCheckMate = rules.checkForThreatens && newBoard.isLossFor(m.fromPiece.enemy)
      lazy val isCheck = rules.checkForThreatens && m.fromPiece.enemy.kingPiece(newBoard).exists(_.isThreatened(newBoard))

      Set(m) filter (_ ⇒ isPlayersKingThreatened) map { _ ⇒
        val mate = isCheckMate
        val check = mate || isCheck

        mf.complete(check, mate)
      }
    }

    validateMovement flatMap validateAfterMovement
  }

  def isDrawFor(player: ChessPlayer)(implicit rules: ChessRules) = player.movements(this).isEmpty && !isLossFor(player)
  def isLossFor(player: ChessPlayer)(implicit rules: ChessRules): Boolean = {
    val noCheckForMates = rules.copy(checkForThreatens = false)
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
