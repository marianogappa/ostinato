package ostinato.chess.core

import ostinato.core.{ XY, Board }

case class ChessBoard(
    override val grid: Vector[Option[ChessPiece]],
    turn: ChessPlayer = WhiteChessPlayer,
    enPassantPawn: Option[EnPassantPawn] = None,
    castlingAvailable: Map[(ChessPlayer, CastlingSide.Value), Boolean] = castlingFullyAvailable,
    fullMoveNumber: Int = 1,
    halfMoveClock: Int = 0) extends Board[ChessBoard, ChessAction, ChessPiece, ChessPlayer, ChessRules](grid) {

  def doAction(a: ChessAction)(implicit rules: ChessRules = ChessRules.default) = {
    def calculateEnPassants = a match {
      case EnPassantAction(pawn, delta, _, _) ⇒
        Some(EnPassantPawn(pawn.pos + XY(0, math.signum(delta.y)), pawn.movedTo(pawn.pos + XY(0, delta.y))))
      case _ ⇒
        None
    }

    def calculateCastlingAvailable = a match {
      case CastlingAction(_, _, _, _, _, _) ⇒
        castlingAvailable.updated((turn, CastlingSide.Queenside), false).updated((turn, CastlingSide.Kingside), false)
      case _ ⇒
        castlingAvailable
    }

    def calculateHalfMoveClock = a match {
      case MoveAction(p: ♟, _, _, _)              ⇒ 0
      case EnPassantAction(_, _, _, _)            ⇒ 0
      case EnPassantCaptureAction(_, _, _, _, _)  ⇒ 0
      case CaptureAction(_, _, _, _, _)           ⇒ 0
      case PromoteAction(_, _, _, _, _)           ⇒ 0
      case CapturePromoteAction(_, _, _, _, _, _) ⇒ 0
      case _                                      ⇒ halfMoveClock + 1
    }

    get(a.fromPiece.pos) match {
      case Some(Some(a.fromPiece)) if a.fromPiece.owner == turn ⇒
        Some(ChessBoard(
          a.gridUpdates.foldLeft(grid)(applyUpdate),
          turn.enemy,
          calculateEnPassants,
          calculateCastlingAvailable,
          if (turn == BlackChessPlayer) fullMoveNumber + 1 else fullMoveNumber,
          calculateHalfMoveClock
        ))
      case _ ⇒
        None
    }
  }

  def action(from: XY, delta: XY)(implicit rules: ChessRules = ChessRules.default): Set[ChessAction] = {
    val to = from + delta
    val fromPiece = get(from)
    val toPiece = get(to)
    lazy val betweenLocationsFree = between(from, to) forall isEmptyCell
    def betweenLocationsNotThreatenedBy(player: ChessPlayer) =
      xyBetween(from, to) forall (pos ⇒ player.pieces(this) forall (!_.canMoveTo(pos, this.copy(turn = player))))

    def isEnPassantPawn(pos: XY) = enPassantPawn.exists(epp ⇒ epp.from == pos)

    def targetRook(k: ♚) = get(k.targetRookPosition(delta.x)) match {
      case Some(Some(r: ♜)) if r.owner == k.owner && r.castlingSide.nonEmpty ⇒ Some((r, r.castlingSide))
      case _ ⇒ None
    }

    val validateAction: Set[ChessActionFactory] = (fromPiece, toPiece, enPassantPawn) match {
      case (Some(Some(p: ♟)), Some(None), Some(epp: EnPassantPawn)) if delta.x != 0 && isEnPassantPawn(to) && epp.pawn.owner != p.owner ⇒
        Set(EnPassantTakeActionFactory(p, delta, epp.pawn))

      case (Some(Some(p: ♟)), Some(None), _) if delta.x == 0 && math.abs(delta.y) == 2 && betweenLocationsFree ⇒
        Set(EnPassantActionFactory(p, delta))

      case (Some(Some(p: ♟)), Some(Some(toP: ChessPiece)), _) if delta.x != 0 && (!toP.isKing || rules.kingIsTakeable) && toP.owner != p.owner && to.y == ♟.promotingPosition(delta.y) ⇒
        Set(
          ♜(from + delta, p.owner), ♝(from + delta, p.owner),
          ♞(from + delta, p.owner), ♛(from + delta, p.owner)) map (CapturePromoteActionFactory(p, delta, toP, _))

      case (Some(Some(p: ♟)), Some(None), _) if delta.x == 0 && math.abs(delta.y) == 1 && to.y == ♟.promotingPosition(delta.y) ⇒
        Set(
          ♜(from + delta, p.owner), ♝(from + delta, p.owner),
          ♞(from + delta, p.owner), ♛(from + delta, p.owner)) map (PromoteActionFactory(p, delta, _))

      case (Some(Some(p: ♟)), Some(None), _) if delta.x == 0 && math.abs(delta.y) == 1 ⇒
        Set(MoveActionFactory(p, delta))

      case (Some(Some(p: ♟)), Some(Some(toP: ChessPiece)), _) if delta.x != 0 && (!toP.isKing || rules.kingIsTakeable) && toP.owner != p.owner ⇒
        Set(CaptureActionFactory(p, delta, toP))

      case (Some(Some(k: ♚)), _, _) if math.abs(delta.x) == 2 ⇒
        (toPiece, targetRook(k)) match {
          case (Some(None), Some((r: ♜, Some(cs: CastlingSide.Value)))) if k.isInInitialPosition &&
            castlingAvailable((k.owner, cs)) && betweenLocationsFree && !k.isThreatened(this) &&
            betweenLocationsNotThreatenedBy(k.enemy) ⇒

            Set(CastlingActionFactory(k, delta, r, ♚.rookDeltaFor(delta)))

          case _ ⇒ Set()
        }

      case (Some(Some(p: ChessPiece)), Some(None), _) if !p.isPawn && betweenLocationsFree ⇒
        Set(MoveActionFactory(p, delta))

      case (Some(Some(p: ChessPiece)), Some(Some(toP: ChessPiece)), _) if !p.isPawn && betweenLocationsFree && (!toP.isKing || rules.kingIsTakeable) && toP.owner != p.owner ⇒
        Set(CaptureActionFactory(p, delta, toP))

      case _ ⇒ Set()
    }

    def validateAfterAction(mf: ChessActionFactory): Set[ChessAction] = doAction(mf.complete()).toSet.flatMap { newBoard: ChessBoard ⇒
      val m = mf.complete()
      val isPlayersKingThreatened = rules.checkForThreatens && m.fromPiece.owner.kingPiece(newBoard).exists(_.isThreatened(newBoard))
      lazy val isCheckMate = rules.checkForThreatens && newBoard.isLossFor(m.fromPiece.enemy)
      lazy val isCheck = rules.checkForThreatens && m.fromPiece.enemy.kingPiece(newBoard).exists(_.isThreatened(newBoard))

      Set(m) filter (_ ⇒ !isPlayersKingThreatened) map { _ ⇒
        val mate = isCheckMate
        val check = mate || isCheck

        mf.complete(check, mate)
      }
    }

    validateAction flatMap validateAfterAction
  }

  def isDraw(implicit rules: ChessRules = ChessRules.default) = isDrawFor(turn)
  def isDrawFor(player: ChessPlayer)(implicit rules: ChessRules = ChessRules.default) =
    player.nonWinDrawActions(this).isEmpty && !isLossFor(player)

  def isLoss(implicit rules: ChessRules = ChessRules.default) = isLossFor(turn)
  def isLossFor(player: ChessPlayer)(implicit rules: ChessRules = ChessRules.default): Boolean = {
    val noCheckForMates = rules.copy(checkForThreatens = false)
    lazy val allNewBoards = player.actions(this)(noCheckForMates) map doAction
    def isKingThreatened(b: ChessBoard): Boolean = player.kingPiece(b).exists(_.isThreatened(b)(noCheckForMates))

    player.kingPiece(this).map { _.isThreatened(this)(noCheckForMates) && (allNewBoards.flatten forall isKingThreatened) } getOrElse
      rules.noKingMeansLoss
  }

  override def toString: String = {
    def cellToChar(cell: Cell): Char = cell map (_.toFigurine) getOrElse '.'
    val linesOfCells = grid.grouped(8) map (_.toList)

    linesOfCells map (_ map cellToChar) map (_.mkString) mkString "\n"
  }

  def toFen: String = grid.map {
    case Some(c) ⇒ c.toFen
    case _       ⇒ ' '
  }.foldLeft(Fen(""))(Fen.+).toString

  def nonWinDrawActions(implicit rules: ChessRules = ChessRules.default) =
    actions.filter {
      case WinAction(_)        ⇒ false
      case DrawAction(_, _, _) ⇒ false
      case _                   ⇒ true
    }

  def actions(implicit rules: ChessRules = ChessRules.default) = turn.actions(this)
  def rooks = pieces filter (_.isRook)
  def knights = pieces filter (_.isKnight)
  def bishops = pieces filter (_.isBishop)
  def queens = pieces filter (_.isQueen)
  def kings = pieces filter (_.isKing)
  def pawns = pieces filter (_.isPawn)
  def game(implicit rules: ChessRules = ChessRules.default) = ChessGame(this, rules)
}
