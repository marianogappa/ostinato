package ostinato.chess.core

object An {
  def allPossibleAns(m: ChessAction)(implicit rules: ChessRules = ChessRules.default): Set[String] = {
    def checkAndCheckMate(isCheck: Boolean, isCheckMate: Boolean) =
      if (isCheckMate)
        Set("#", "++", "mate", "‡", "≠")
      else if (isCheck)
        Set("+", "†", "ch", "dbl ch", "++", "") // TODO dbl ch and ++ should be calculated
      else
        Set("")

    m match {
      case m @ CaptureAction(fromPiece, delta, toPiece, isCheck, isCheckMate) ⇒
        for {
          from ← if (fromPiece.isPawn) Set(fromPiece.pos.toAn.x.toString) else Set(fromPiece.toAn, fromPiece.toChar) // TODO toChar -> toFigurine
          fromPos ← Set("", fromPiece.pos.toAn.x, fromPiece.pos.toAn)
          capture ← Set("x", ":", "")
          toPos ← Set((fromPiece.pos + delta).toAn, (fromPiece.pos + delta).toAn.x.toString)
          check ← checkAndCheckMate(isCheck, isCheckMate)
          suffixCapture ← if (capture == "") Set(":", "") else Set("")
        } yield from + fromPos.toString + capture + toPos + suffixCapture + check
      case m @ MoveAction(fromPiece, delta, isCheck, isCheckMate) ⇒
        for {
          from ← if (fromPiece.isPawn) Set("") else Set(fromPiece.toAn, fromPiece.toChar) // TODO toChar -> toFigurine
          fromPos ← Set("", fromPiece.pos.toAn.x, fromPiece.pos.toAn)
          toPos ← Set((fromPiece.pos + delta).toAn)
          check ← checkAndCheckMate(isCheck, isCheckMate)
        } yield from + fromPos.toString + toPos + check
      case m @ EnPassantCaptureAction(fromPawn, delta, toPawn, isCheck, isCheckMate) ⇒
        for {
          from ← Set(fromPawn.pos.toAn.x.toString) // TODO toChar -> toFigurine
          deltaPos ← Set((fromPawn.pos + delta).toAn)
          enPassant ← Set("e.p.", "")
          check ← checkAndCheckMate(isCheck, isCheckMate)
        } yield from + deltaPos + enPassant + check
      case m @ EnPassantAction(fromPiece, delta, isCheck, isCheckMate) ⇒
        for {
          from ← Set("") // TODO toChar -> toFigurine
          fromPos ← Set("", fromPiece.pos.toAn.x, fromPiece.pos.toAn)
          toPos ← Set((fromPiece.pos + delta).toAn)
          check ← checkAndCheckMate(isCheck, isCheckMate)
        } yield from + fromPos.toString + toPos + check
      case m @ PromoteAction(fromPiece, delta, toPiece, isCheck, isCheckMate) ⇒
        for {
          fromPos ← Set("", fromPiece.pos.toAn.x, fromPiece.pos.toAn.toString)
          toPos ← Set((fromPiece.pos + delta).toAn.toString)
          promotion ← Set("", "=", "/", "(")
          to ← Set(toPiece.toAn, toPiece.toChar) // TODO toChar -> toFigurine
          promotionSuffix ← if (promotion == "(") Set(")") else Set("")
          check ← checkAndCheckMate(isCheck, isCheckMate)
        } yield fromPos + toPos + promotion + to + promotionSuffix + check
      case m @ CastlingAction(fromPiece, kingDelta, targetRook, rookDelta, isCheck, isCheckMate) if kingDelta.x == -2 ⇒
        for {
          castling ← if (kingDelta.x == -2) Set("0-0-0", "O-O-O") else Set("0-0", "O-O")
          check ← checkAndCheckMate(isCheck, isCheckMate)
        } yield castling + check
      case m @ DrawAction(fromPlayer, isCheck, isCheckMate) ⇒
        Set("½–½")
    }
  }
}
