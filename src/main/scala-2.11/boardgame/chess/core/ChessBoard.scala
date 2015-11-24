package boardgame.chess.core

import boardgame.core.{ XY, Board }

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

      case PromoteMovement(_, toPiece) ⇒
        List((toPiece.pos.toI, Some(toPiece)))

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
      case _                                      ⇒ None
    }

    val validateMovement: Option[ChessMovement] = (fromPiece, toPiece, enPassantPawn) match {
      case (Some(Some(p: ♟)), Some(None), Some(epp: EnPassantPawn)) if delta.x != 0 && isEnPassantPawn(to) && epp.pawn.owner != p.owner ⇒
        Some(EnPassantTakeMovement(p, delta, epp.pawn))

      case (Some(Some(p: ♟)), Some(None), _) if delta.x == 0 && math.abs(delta.y) == 2 && betweenLocationsFree ⇒
        Some(EnPassantMovement(p, delta))

      case (Some(Some(p: ♟)), Some(None), _) if delta.x == 0 && math.abs(delta.y) == 1 && to.y == ♟.promotingPosition(delta.y) ⇒
        Some(ReachPromoteMovement(p, delta))

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
