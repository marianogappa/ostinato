package ostinato.chess

import ostinato.chess.core.{ChessBoard, WhiteChessPlayer, BlackChessPlayer, ChessGame}
import org.scalatest._

import scala.util.Random

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

      val board2 = board.doAction(board.nonWinDrawActions.head).get
      board2.turn shouldBe BlackChessPlayer
      board2.fullMoveNumber shouldBe 1

      val board3 = board2.doAction(board2.nonWinDrawActions.head).get
      board3.turn shouldBe WhiteChessPlayer
      board3.fullMoveNumber shouldBe 2
    }

    it("should change turn after an action when on white's turn") {
      val board = ChessGame.defaultGame.board
      board.doAction(board.nonWinDrawActions.head).get.turn shouldBe BlackChessPlayer
    }

    it("should change turn after a action when on blacks's turn") {
      val game = ChessGame.fromString("""♜♞♝♛♚♝♞♜
                                        |♟♟♟♟♟♟♟♟
                                        |........
                                        |........
                                        |........
                                        |........
                                        |♙♙♙♙♙♙♙♙
                                        |♖♘♗♕♔♗♘♖""".stripMargin, turn = BlackChessPlayer)

      game.board.doAction(game.board.nonWinDrawActions.head).get.turn shouldBe WhiteChessPlayer
    }

    it("should play a lot and not stack overflow") {
      def doActionsUntilLockedOrNActions(board: ChessBoard = ChessGame.defaultGame.board, n: Int = 200): Unit = {
        val actions = board.nonWinDrawActions
        if (n > 0 && actions.nonEmpty)
          doActionsUntilLockedOrNActions(board.doAction(actions.head).get, n - 1)
        else
          ()
      }

      doActionsUntilLockedOrNActions()
    }
  }
}
