package ostinato.chess.core

import org.scalatest._
import ostinato.core.XY

class ChessBoardTest extends FunSpec with ShouldMatchers {
  describe("ChessBoard") {
    it("should increase the half move clock count after a non capture nor pawn advance") {
      val game = ChessGame.defaultGame
      val board = game.board

      board.halfMoveClock shouldBe 0

      val board2 = board.doAction(game.whitePlayer.knights(board).head.actions(board).head).get

      board2.halfMoveClock shouldBe 1
    }

    it("should zero the half move clock count after a Pawn movement") {
      val game = ChessGame.defaultGame
      val board = game.board

      board.halfMoveClock shouldBe 0

      val board2 = board.doAction(game.whitePlayer.pawns(board).head.actions(board).head).get

      board2.halfMoveClock shouldBe 0
    }

    it("should increment the full move count after a black move") {
      val board = ChessGame.defaultGame.board
      board.turn shouldBe WhiteChessPlayer
      board.fullMoveNumber shouldBe 1

      val board2 = board.doAction(board.nonFinalActions.head).get
      board2.turn shouldBe BlackChessPlayer
      board2.fullMoveNumber shouldBe 1

      val board3 = board2.doAction(board2.nonFinalActions.head).get
      board3.turn shouldBe WhiteChessPlayer
      board3.fullMoveNumber shouldBe 2
    }

    it("should change turn after an action when on white's turn") {
      val board = ChessGame.defaultGame.board
      board.doAction(board.nonFinalActions.head).get.turn shouldBe BlackChessPlayer
    }

    it("should change turn after a action when on blacks's turn") {
      val game = ChessGame.fromGridString("""♜♞♝♛♚♝♞♜
                                        |♟♟♟♟♟♟♟♟
                                        |........
                                        |........
                                        |........
                                        |........
                                        |♙♙♙♙♙♙♙♙
                                        |♖♘♗♕♔♗♘♖""".stripMargin, turn = BlackChessPlayer).get

      game.board.doAction(game.board.nonFinalActions.head).get.turn shouldBe WhiteChessPlayer
    }

    it("should play a lot and not stack overflow") {
      def doActionsUntilLockedOrNActions(board: ChessBoard = ChessGame.defaultGame.board, n: Int = 200): Unit = {
        val actions = board.nonFinalActions
        if (n > 0 && actions.nonEmpty)
          doActionsUntilLockedOrNActions(board.doAction(actions.head).get, n - 1)
        else
          ()
      }

      doActionsUntilLockedOrNActions()
    }
  }
  describe("Square colors") {
    it("should calculate square colors properly") {
      Set(XY(0, 0), XY(0, 2), XY(0, 4), XY(0, 6), XY(1, 1), XY(1, 3), XY(1, 5), XY(1, 7), XY(2, 0), XY(2, 2), XY(2, 4),
        XY(2, 6), XY(3, 1), XY(3, 3), XY(3, 5), XY(3, 7), XY(4, 0), XY(4, 2), XY(4, 4), XY(4, 6), XY(5, 1), XY(5, 3),
        XY(5, 5), XY(5, 7), XY(6, 0), XY(6, 2), XY(6, 4), XY(6, 6), XY(7, 1), XY(7, 3), XY(7, 5), XY(7, 7)
      ).map(_.squareColor) shouldBe Set(SquareColor.Light)

      Set(XY(0, 1), XY(0, 3), XY(0, 5), XY(0, 7), XY(1, 0), XY(1, 2), XY(1, 4), XY(1, 6), XY(2, 1), XY(2, 3), XY(2, 5),
        XY(2, 7), XY(3, 0), XY(3, 2), XY(3, 4), XY(3, 6), XY(4, 1), XY(4, 3), XY(4, 5), XY(4, 7), XY(5, 0), XY(5, 2),
        XY(5, 4), XY(5, 6), XY(6, 1), XY(6, 3), XY(6, 5), XY(6, 7), XY(7, 0), XY(7, 2), XY(7, 4), XY(7, 6)
      ).map(_.squareColor) shouldBe Set(SquareColor.Dark)
    }
  }
}
