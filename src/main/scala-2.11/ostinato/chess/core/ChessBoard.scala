package ostinato.chess.core

import ostinato.core.{ XY, Board }

case class ChessBoard(
    override val grid: Vector[Option[ChessPiece]],
    turn: ChessPlayer = WhiteChessPlayer,
    enPassantPawn: Option[EnPassantPawn] = None,
    castlingAvailable: Map[(ChessPlayer, CastlingSide.Value), Boolean] = castlingFullyAvailable,
    fullMoveNumber: Int = 1,
    halfMoveClock: Int = 0,
    pastBoards: PastBoards = PastBoards()) extends Board[ChessBoard, ChessAction, ChessPiece, ChessPlayer, ChessRules](grid) {

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

    def calculatePastBoards(newBoard: ChessBoard) =
      if (pastBoards.isEmpty)
        pastBoards.withBoard(this).withBoard(newBoard)
      else
        pastBoards.withBoard(newBoard)

    lazy val applyAction = {
      val newBoard = ChessBoard(
        a.gridUpdates.foldLeft(grid)(applyUpdate),
        turn.enemy,
        calculateEnPassants,
        calculateCastlingAvailable,
        if (turn == BlackChessPlayer) fullMoveNumber + 1 else fullMoveNumber,
        calculateHalfMoveClock
      )
      Some(newBoard.copy(pastBoards = calculatePastBoards(newBoard)))
    }

    lazy val applyFinalAction = Some(ChessBoard(grid, turn, None, castlingFullyUnavailable, fullMoveNumber, 0))

    if (rules.extraValidationOnActionApply) {
      (a, get(a.fromPiece.pos)) match {
        case (action, _) if action.isFinal ⇒
          applyFinalAction
        case (_, Some(Some(a.fromPiece))) if a.fromPiece.owner == turn ⇒
          applyAction
        case _ ⇒
          None
      }
    } else {
      if (a.isFinal)
        applyFinalAction
      else
        applyAction
    }
  }

  def movementsOfDelta(from: XY, delta: XY)(implicit rules: ChessRules = ChessRules.default): Set[ChessAction] = {
    val to = from + delta
    val fromPiece = get(from)
    val toPiece = get(to)
    def betweenLocationsFree(f: XY = from, t: XY = to) = between(f, t) forall isEmptyCell
    def betweenLocationsNotThreatenedBy(player: ChessPlayer) =
      xyBetween(from, to) forall (pos ⇒ player.pieces(this) forall (!_.canMoveTo(pos, this.copy(turn = player))))

    def isEnPassantPawn(pos: XY) = enPassantPawn.exists(epp ⇒ epp.from == pos)

    def targetRook(k: ♚) = get(k.targetRookPosition(delta.x)) match {
      case Some(Some(r: ♜)) if r.owner == k.owner && r.castlingSide.nonEmpty ⇒ Some((r, r.castlingSide))
      case _ ⇒ None
    }

    lazy val validateAction: Set[ChessActionFactory] = (fromPiece, toPiece, enPassantPawn) match {
      case (Some(Some(p: ♟)), Some(None), Some(epp: EnPassantPawn)) if delta.x != 0 && isEnPassantPawn(to) && epp.pawn.owner != p.owner ⇒
        Set(EnPassantTakeActionFactory(p, delta, epp.pawn))

      case (Some(Some(p: ♟)), Some(None), _) if delta.x == 0 && math.abs(delta.y) == 2 && betweenLocationsFree() ⇒
        Set(EnPassantActionFactory(p, delta))

      case (Some(Some(p: ♟)), Some(Some(toP: ChessPiece)), _) if delta.x != 0 && (!toP.isKing || rules.kingIsTakeable) && toP.owner != p.owner && to.y == p.promotingPosition(delta.y) ⇒
        Set(
          ♜(from + delta, p.owner), ♝(from + delta, p.owner),
          ♞(from + delta, p.owner), ♛(from + delta, p.owner)) map (CapturePromoteActionFactory(p, delta, toP, _))

      case (Some(Some(p: ♟)), Some(None), _) if delta.x == 0 && math.abs(delta.y) == 1 && to.y == p.promotingPosition(delta.y) ⇒
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
            castlingAvailable((k.owner, cs)) && betweenLocationsFree() &&
            betweenLocationsFree(r.pos, r.pos + k.rookDeltaFor(delta)) && !k.isThreatened(this) &&
            betweenLocationsNotThreatenedBy(k.enemy) ⇒

            Set(CastlingActionFactory(k, delta, r, k.rookDeltaFor(delta)))

          case _ ⇒ Set()
        }

      case (Some(Some(p: ChessPiece)), Some(None), _) if !p.isPawn && betweenLocationsFree() ⇒
        Set(MoveActionFactory(p, delta))

      case (Some(Some(p: ChessPiece)), Some(Some(toP: ChessPiece)), _) if !p.isPawn && betweenLocationsFree() && (!toP.isKing || rules.kingIsTakeable) && toP.owner != p.owner ⇒
        Set(CaptureActionFactory(p, delta, toP))

      case _ ⇒ Set()
    }

    def validateAfterAction(mf: ChessActionFactory): Set[ChessAction] = doAction(mf.complete()).toSet.flatMap { newBoard: ChessBoard ⇒
      val m = mf.complete()
      val isPlayersKingThreatened = rules.checkForThreatens && m.fromPiece.owner.kingPiece(newBoard).exists(_.isThreatened(newBoard))
      lazy val isCheck = rules.checkForThreatens && m.fromPiece.enemy.kingPiece(newBoard).exists(_.isThreatened(newBoard))

      Set(m) filter (_ ⇒ !isPlayersKingThreatened) map { _ ⇒
        val check = isCheck
        val mate = check && newBoard.isLossFor(m.fromPiece.enemy, basedOnCheckKnown = true)

        mf.complete(check, mate)
      }
    }

    lazy val concreteMovementsOfDelta = validateAction flatMap validateAfterAction

    if (rules.validateDeltasOnActionCalculation)
      fromPiece match {
        case Some(Some(p: ChessPiece)) if p.deltas(this).contains(delta) ⇒ concreteMovementsOfDelta
        case _ ⇒ Set()
      }
    else
      concreteMovementsOfDelta
  }

  def isDraw(implicit rules: ChessRules = ChessRules.default) = isDrawFor(turn)
  def isDrawFor(player: ChessPlayer)(implicit rules: ChessRules = ChessRules.default) =
    player.nonFinalActions(this).isEmpty && !isLossFor(player)

  def isLoss(implicit rules: ChessRules = ChessRules.default) = isLossFor(turn)
  def isLossFor(player: ChessPlayer, basedOnCheckKnown: Boolean = false)(implicit rules: ChessRules = ChessRules.default): Boolean = {
    val noCheckForMates = rules.copy(checkForThreatens = false)
    lazy val allNewBoards = player.actions(this)(noCheckForMates) map doAction
    def isKingThreatened(b: ChessBoard): Boolean = player.kingPiece(b).exists(_.isThreatened(b)(noCheckForMates))

    player.kingPiece(this).map { king ⇒
      (basedOnCheckKnown || king.isThreatened(this)(noCheckForMates)) && (allNewBoards.flatten forall isKingThreatened)
    } getOrElse rules.noKingMeansLoss
  }

  override def toString: String = {
    def cellToChar(cell: Cell): Char = cell map (_.toFigurine) getOrElse '.'
    val linesOfCells = grid.grouped(8) map (_.toList)

    linesOfCells map (_ map cellToChar) map (_.mkString) mkString "\n"
  }

  def toShortFen: String = grid.map {
    case Some(c) ⇒ c.toFen
    case _       ⇒ ' '
  }.foldLeft(Fen(""))(Fen.+).toString

  def toFen: String =
    toShortFen + " " +
      turn.toFen + " " +
      fenCastling(castlingAvailable) + " " +
      enPassantPawn.map(_.from.toAn).getOrElse("-") + " " +
      halfMoveClock + " " +
      fullMoveNumber

  private def simpleInsufficientMaterial =
    Set("Kk", "Kbk", "KNk", "BKk", "Kkn") contains pieces.map(_.toFen).mkString.sorted

  private def kingsBishopsInsufficientMaterial =
    "BKbk" == pieces.map(_.toFen).toSet.mkString.sorted && bishops.map(_.pos.squareColor).toSet.size == 1

  def hasInsufficientMaterial = simpleInsufficientMaterial || kingsBishopsInsufficientMaterial
  def isInFiftyMoveRule = halfMoveClock >= 50
  def isInThreefoldRepetition = pastBoards.isInThreefoldRepetition
  def isInStalemate = turn.actions(this) == Set(DrawAction(turn, isCheck = false, isCheckmate = false))

  def nonFinalActions(implicit rules: ChessRules = ChessRules.default) = turn.nonFinalActions(this)
  def actions(implicit rules: ChessRules = ChessRules.default) = turn.actions(this)
  def actionStream(implicit rules: ChessRules = ChessRules.default) = turn.actionStream(this)
  def rooks: Vector[♜] = pieces flatMap { case p: ♜ ⇒ Vector(p); case _ ⇒ Vector() }
  def knights: Vector[♞] = pieces flatMap { case p: ♞ ⇒ Vector(p); case _ ⇒ Vector() }
  def bishops: Vector[♝] = pieces flatMap { case p: ♝ ⇒ Vector(p); case _ ⇒ Vector() }
  def queens: Vector[♛] = pieces flatMap { case p: ♛ ⇒ Vector(p); case _ ⇒ Vector() }
  def kings: Vector[♚] = pieces flatMap { case p: ♚ ⇒ Vector(p); case _ ⇒ Vector() }
  def pawns: Vector[♟] = pieces flatMap { case p: ♟ ⇒ Vector(p); case _ ⇒ Vector() }
  def game(implicit rules: ChessRules = ChessRules.default) = ChessGame(this, rules)

  override def equals(any: Any) = any match {
    case that: ChessBoard ⇒
      that.grid == grid &&
        that.turn == turn &&
        that.castlingAvailable == castlingAvailable &&
        that.enPassantPawn == enPassantPawn &&
        that.fullMoveNumber == fullMoveNumber &&
        that.halfMoveClock == halfMoveClock
    case _ ⇒
      false
  }
}
