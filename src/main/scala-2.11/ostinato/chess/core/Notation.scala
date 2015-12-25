package ostinato.chess.core

import scala.annotation.tailrec

object Notation {
  def allPossibleNotations(a: ChessAction)(implicit rules: ChessRules = ChessRules.default): Set[String] = {
    def checkAndCheckMate(isCheck: Boolean, isCheckMate: Boolean) =
      if (isCheckMate)
        Set("#", "++", "mate", "‡", "≠")
      else if (isCheck)
        Set("+", "†", "ch", "dbl ch", "++", "") // TODO dbl ch and ++ should be calculated
      else
        Set("")

    implicit class CartesianProductableString(s: String) {
      def *(that: String) = Set(s + that)
      def *(that: Set[String]) = for {
        i1 ← Set(s)
        i2 ← that
      } yield i1 + i2
    }
    implicit class CartesianProductableStringSet(s: Set[String]) {
      def *(that: String) = for {
        i1 ← s
        i2 ← Set(that)
      } yield i1 + i2
      def *(that: Set[String]) = for {
        i1 ← s
        i2 ← that
      } yield i1 + i2
    }

    def genericPromotion(toPiece: ChessPiece) =
      Set(toPiece.toAn.toString, toPiece.toFigurine.toString, s"(${toPiece.toAn})", s"(${toPiece.toFigurine})",
        s"=${toPiece.toAn}", s"=${toPiece.toFigurine}", s"/${toPiece.toAn}", s"/${toPiece.toFigurine}")

    def genericCastling(a: CastlingAction) =
      if (a.isKingside)
        Set("0-0", "O-O")
      else
        Set("0-0-0", "O-O")

    def genericDraw(a: DrawAction) = Set("½–½", "draws")

    def iccfAction(a: ChessAction) =
      a.fromPiece.pos.toIccf.toString * (a.fromPiece.pos + a.delta).toIccf.toString

    def iccfPromote(a: PromoteAction) =
      a.fromPiece.pos.toIccf.toString * (a.fromPiece.pos + a.delta).toIccf.toString * a.toPiece.toIccf.toString

    def canCapture(a: CaptureAction) =
      Set(a.fromPiece.pos.toAn.toString.toLowerCase, a.fromPiece.pos.toAn.toString.toUpperCase) *
        Set("x", ":", "-") *
        Set((a.fromPiece.pos + a.delta).toAn.toString.toLowerCase, (a.fromPiece.pos + a.delta).toAn.toString.toUpperCase) *
        checkAndCheckMate(a.isCheck, a.isCheckmate)

    def canAction(a: ChessAction) =
      Set(a.fromPiece.pos.toAn.toString.toLowerCase, a.fromPiece.pos.toAn.toString.toUpperCase) *
        "-" *
        Set((a.fromPiece.pos + a.delta).toAn.toString.toLowerCase, (a.fromPiece.pos + a.delta).toAn.toString.toUpperCase) *
        checkAndCheckMate(a.isCheck, a.isCheckmate)

    def canPromote(a: PromoteAction) =
      Set(a.fromPiece.pos.toAn.toString.toLowerCase, a.fromPiece.pos.toAn.toString.toUpperCase) *
        "-" *
        Set((a.fromPiece.pos + a.delta).toAn.toString.toLowerCase, (a.fromPiece.pos + a.delta).toAn.toString.toUpperCase) *
        genericPromotion(a.toPiece) *
        checkAndCheckMate(a.isCheck, a.isCheckmate)

    def smithMove(a: ChessAction) =
      a.fromPiece.pos.toAn.toString * (a.fromPiece.pos + a.delta).toAn.toString

    def smithCapture(a: CaptureAction) =
      a.fromPiece.pos.toAn.toString * (a.fromPiece.pos + a.delta).toAn.toString * a.toPiece.toDn.map(_.toLowerCase)

    def smithEnPassantCapture(a: EnPassantCaptureAction) =
      a.fromPiece.pos.toAn.toString * (a.fromPiece.pos + a.delta).toAn.toString * a.toPawn.toDn.map(_.toLowerCase)

    def smithCastling(a: CastlingAction) =
      a.fromPiece.pos.toAn.toString * (a.fromPiece.pos + a.delta).toAn.toString * (if (a.isKingside) "c" else "C")

    def smithPromote(a: PromoteAction) =
      a.fromPiece.pos.toAn.toString * (a.fromPiece.pos + a.delta).toAn.toString * a.toPiece.toAn

    def descriptiveMove(a: ChessAction) =
      (a.fromPiece.toDn * Set("-", "") * (a.fromPiece.pos + a.delta).toDn(a.turn)) * checkAndCheckMate(a.isCheck, a.isCheckmate)

    // TODO Descriptive actions with disambiguations
    def descriptiveCapture(a: CaptureAction) = // TODO with pos
      a.fromPiece.toDn * "x" * a.toPiece.toDn * checkAndCheckMate(a.isCheck, a.isCheckmate)

    def descriptiveCastling(a: CastlingAction) =
      (genericCastling(a) ++ Set("Castles", "castles")) * checkAndCheckMate(a.isCheck, a.isCheckmate)

    def anMove(a: ChessAction) =
      Set(a.fromPiece.toAn.toString, a.fromPiece.toFigurine.toString) *
        Set("", a.fromPiece.pos.toAn.x.toString, a.fromPiece.pos.toAn.toString) *
        (a.fromPiece.pos + a.delta).toAn.toString *
        checkAndCheckMate(a.isCheck, a.isCheckmate)

    def anPromote(a: PromoteAction) =
      Set("", a.fromPiece.pos.toAn.x.toString, a.fromPiece.pos.toAn.toString) *
        (a.fromPiece.pos + a.delta).toAn.toString *
        genericPromotion(a.toPiece) *
        checkAndCheckMate(a.isCheck, a.isCheckmate)

    // TODO Missing : at the end as an option
    def anCapture(a: CaptureAction) =
      (if (a.fromPiece.isPawn) Set(a.fromPiece.pos.toAn.x.toString) else Set(a.fromPiece.toAn.toString, a.fromPiece.toFigurine.toString)) *
        Set("", a.fromPiece.pos.toAn.x.toString, a.fromPiece.pos.toAn.toString) *
        Set("x", ":", "") *
        Set((a.fromPiece.pos + a.delta).toAn.toString, (a.fromPiece.pos + a.delta).toAn.x.toString) *
        checkAndCheckMate(a.isCheck, a.isCheckmate)

    def anEnPassantCapture(a: EnPassantCaptureAction) =
      a.fromPawn.pos.toAn.x.toString *
        (a.fromPawn.pos + a.delta).toAn.toString *
        Set("e.p.", "") *
        checkAndCheckMate(a.isCheck, a.isCheckmate)

    a match {
      case a: CaptureAction ⇒
        iccfAction(a) ++ canCapture(a) ++ smithCapture(a) ++ descriptiveCapture(a) ++ anCapture(a)
      case a: MoveAction ⇒
        iccfAction(a) ++ canAction(a) ++ smithMove(a) ++ descriptiveMove(a) ++ anMove(a)
      case a: EnPassantCaptureAction ⇒
        iccfAction(a) /*++ canCapture(a)*/ ++ smithEnPassantCapture(a) /*++ descriptiveCapture(a)*/ ++ anEnPassantCapture(a)
      case a: EnPassantAction ⇒
        iccfAction(a) ++ canAction(a) ++ smithMove(a) ++ descriptiveMove(a) ++ anMove(a)
      case a: PromoteAction ⇒
        iccfPromote(a) ++ canPromote(a) ++ smithPromote(a) ++ anPromote(a)
      case a: CastlingAction ⇒
        iccfAction(a) ++ genericCastling(a) ++ smithCastling(a) ++ descriptiveCastling(a)
      case a: DrawAction ⇒
        genericDraw(a)
    }
  }

  private def prepareMatchString(s: String) =
    s.replaceAll("""\s+|\d+\.|\[[^\]]*\]""", " ").replaceAll(" +", " ").replaceAll("""[\?!]*""", "").trim.split(' ')

  @tailrec
  private def doParseMatch(actions: List[String], currentBoard: ChessBoard, states: List[(String, Option[(ChessAction, ChessBoard)])])(
    implicit rules: ChessRules = ChessRules.default): Either[List[(String, Option[(ChessAction, ChessBoard)])], List[(ChessAction, ChessBoard)]] =
    actions match {
      case Nil ⇒
        Right(states collect { case (_, Some(s)) => s })
      case a :: as ⇒
        val allPossibleActions = currentBoard.actions.flatMap(a ⇒ a.allPossibleNotations.map((_, a))).toMap
        allPossibleActions.get(a) match {
          case Some(chessAction: ChessAction) ⇒
            currentBoard.doAction(chessAction) match {
              case Some(newBoard: ChessBoard) ⇒
//                println(s"Successfully processed $a with $chessAction", newBoard)
                doParseMatch(as, newBoard, states :+ (a, Some((chessAction, newBoard))))
              case None ⇒
                Left(states ++ (a :: as).map((_, None)))
            }
          case None ⇒
            Left(states ++ (a :: as).map((_, None)))
        }
    }

  def parseMatchString(s: String, board: ChessBoard = ChessGame.defaultGame.board)(implicit rules: ChessRules = ChessRules.default) =
    doParseMatch(prepareMatchString(s).toList, board, List.empty[(String, Option[(ChessAction, ChessBoard)])])
}
