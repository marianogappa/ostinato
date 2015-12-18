package ostinato.chess.core

import ostinato.core.{ XY, Board }

case class ChessBoard(
    override val grid: Vector[Option[ChessPiece]],
    turn: ChessPlayer = WhiteChessPlayer,
    enPassantPawn: Option[EnPassantPawn] = None,
    castlingAvailable: Map[(ChessPlayer, CastlingSide.Value), Boolean] = castlingFullyAvailable,
    fullMoveNumber: Int = 1,
    halfMoveClock: Int = 0) extends Board[ChessPiece, ChessMovement, ChessBoard, ChessRules](grid) {

  def move(m: ChessMovement)(implicit rules: ChessRules = ChessRules.default) = {
    // TODO there is no check that turn == fromPiece.owner

    val resultingEnPassants = m match {
      case EnPassantMovement(pawn, delta, _, _) ⇒
        Some(EnPassantPawn(pawn.pos + XY(0, math.signum(delta.y)), pawn.movedTo(pawn.pos + XY(0, delta.y))))
      case _ ⇒
        None
    }

    val resultingCastlingAvailable = m match {
      case CastlingMovement(_, _, _, _, _, _) ⇒
        castlingAvailable.updated((turn, CastlingSide.Queenside), false).updated((turn, CastlingSide.Kingside), false)
      case _ ⇒
        castlingAvailable
    }

    // TODO DrawMovement should be implemented!
    val resultingHalfMoveClock = m match {
      case MoveMovement(p: ♟, _, _, _)          ⇒ 0
      case EnPassantMovement(_, _, _, _)        ⇒ 0
      case EnPassantTakeMovement(_, _, _, _, _) ⇒ 0
      case TakeMovement(_, _, _, _, _)          ⇒ 0
      case PromoteMovement(_, _, _, _, _)       ⇒ 0
      case _                                    ⇒ halfMoveClock + 1
    }

    ChessBoard(
      m.gridUpdates.foldLeft(grid)(applyUpdate),
      turn.enemy,
      resultingEnPassants,
      resultingCastlingAvailable,
      if (turn == BlackChessPlayer) fullMoveNumber + 1 else fullMoveNumber,
      resultingHalfMoveClock
    )
  }

  def movement(from: XY, delta: XY)(implicit rules: ChessRules = ChessRules.default): Set[ChessMovement] = {
    val to = from + delta
    val fromPiece = get(from)
    val toPiece = get(to)
    lazy val betweenLocationsFree = between(from, to) forall isEmptyCell
    def betweenLocationsNotThreatenedBy(player: ChessPlayer) =
      xyBetween(from, to) forall (pos ⇒ player.pieces(this) forall (!_.canMoveTo(pos, this)))

    def isEnPassantPawn(pos: XY) = enPassantPawn.exists(epp ⇒ epp.from == pos)

    def targetRook(k: ♚) = get(k.targetRookPosition(delta.x)) match {
      case Some(Some(r: ♜)) if r.owner == k.owner && r.castlingSide.nonEmpty ⇒ Some((r, r.castlingSide))
      case _ ⇒ None
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
          case (Some(None), Some((r: ♜, Some(cs: CastlingSide.Value)))) if k.isInInitialPosition &&
            castlingAvailable((k.owner, cs)) && betweenLocationsFree && !k.isThreatened(this) &&
            betweenLocationsNotThreatenedBy(k.enemy) ⇒

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
      val isPlayersKingThreatened = rules.checkForThreatens && m.fromPiece.owner.kingPiece(newBoard).exists(_.isThreatened(newBoard))
      lazy val isCheckMate = rules.checkForThreatens && newBoard.isLossFor(m.fromPiece.enemy)
      lazy val isCheck = rules.checkForThreatens && m.fromPiece.enemy.kingPiece(newBoard).exists(_.isThreatened(newBoard))

      Set(m) filter (_ ⇒ !isPlayersKingThreatened) map { _ ⇒
        val mate = isCheckMate
        val check = mate || isCheck

        mf.complete(check, mate)
      }
    }

    validateMovement flatMap validateAfterMovement
  }

  def isDrawFor(player: ChessPlayer)(implicit rules: ChessRules = ChessRules.default) = player.movements(this).isEmpty && !isLossFor(player)
  def isLossFor(player: ChessPlayer)(implicit rules: ChessRules = ChessRules.default): Boolean = {
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

  def toFen: String = grid.map {
    case Some(c) ⇒ c.toFen
    case _       ⇒ ' '
  }.foldLeft(Fen(""))(Fen.+).toString

  def movements = turn.movements(this)
  def rooks = pieces filter (_.isRook)
  def knights = pieces filter (_.isKnight)
  def bishops = pieces filter (_.isBishop)
  def queens = pieces filter (_.isQueen)
  def kings = pieces filter (_.isKing)
  def pawns = pieces filter (_.isPawn)
  def game(implicit rules: ChessRules) = ChessGame(this, rules)
}
