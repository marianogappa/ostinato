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

      val board2 = board.move(game.whitePlayer.knights(board).head.movements(board).head).get

      board2.halfMoveClock shouldBe 1
    }

    it("should zero the half move clock count after a Pawn movement") {
      val game = ChessGame.defaultGame
      val board = game.board

      board.halfMoveClock shouldBe 0

      val board2 = board.move(game.whitePlayer.pawns(board).head.movements(board).head).get

      board2.halfMoveClock shouldBe 0
    }

    it("should increment the full move count after a black move") {
      val board = ChessGame.defaultGame.board
      board.turn shouldBe WhiteChessPlayer
      board.fullMoveNumber shouldBe 1

      val board2 = board.move(board.movements.head).get
      board2.turn shouldBe BlackChessPlayer
      board2.fullMoveNumber shouldBe 1

      val board3 = board2.move(board2.movements.head).get
      board3.turn shouldBe WhiteChessPlayer
      board3.fullMoveNumber shouldBe 2
    }

    it("should change turn after a movement when on white's turn") {
      val board = ChessGame.defaultGame.board
      board.move(board.movements.head).get.turn shouldBe BlackChessPlayer
    }

    it("should change turn after a movement when on blacks's turn") {
      val game = ChessGame.fromString("""♜♞♝♛♚♝♞♜
                                        |♟♟♟♟♟♟♟♟
                                        |........
                                        |........
                                        |........
                                        |........
                                        |♙♙♙♙♙♙♙♙
                                        |♖♘♗♕♔♗♘♖""".stripMargin, turn = BlackChessPlayer)

      game.board.move(game.board.movements.head).get.turn shouldBe WhiteChessPlayer
    }

    it("should play a lot and not stack overflow") {
      def moveUntilLockedOrNMoves(board: ChessBoard = ChessGame.defaultGame.board, n: Int = 200): Unit = {
        val movements = board.movements
        if (n > 0 && movements.nonEmpty)
          moveUntilLockedOrNMoves(board.move(movements.head).get, n - 1)
        else
          ()
      }

      moveUntilLockedOrNMoves()
    }
  }
}
