package ostinato.chess

import ostinato.chess.core.{ WhiteChessPlayer, BlackChessPlayer, ChessGame }
import org.scalatest._

import scala.util.Random

class ChessBoardTest extends FunSpec with ShouldMatchers {
  describe("ChessBoard") {
    it("should increase the half move clock count after a non capture nor pawn advance") {
      val game = ChessGame.defaultGame
      val board = game.board

      board.halfMoveClock shouldBe 0

      val board2 = board.move(game.whitePlayer.knights(board).head.movements(board).head)

      board2.halfMoveClock shouldBe 1
    }

    it("should zero the half move clock count after a Pawn movement") {
      val game = ChessGame.defaultGame
      val board = game.board

      board.halfMoveClock shouldBe 0

      val board2 = board.move(game.whitePlayer.pawns(board).head.movements(board).head)

      board2.halfMoveClock shouldBe 0
    }

    it("should increment the full move count after a black move") {
      val board = ChessGame.defaultGame.board
      board.turn shouldBe WhiteChessPlayer
      board.fullMoveNumber shouldBe 1

      val board2 = board.move(board.movements.head)
      board2.turn shouldBe BlackChessPlayer
      board2.fullMoveNumber shouldBe 1

      val board3 = board2.move(board.movements.head)
      board3.turn shouldBe WhiteChessPlayer
      board3.fullMoveNumber shouldBe 2
    }

    it("should change turn after a movement when on white's turn") {
      val board = ChessGame.defaultGame.board
      board.move(board.movements.head).turn shouldBe BlackChessPlayer
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

      game.board.move(game.board.movements.head).turn shouldBe WhiteChessPlayer
    }

    ignore("should play a lot and still work") {
      val game = ChessGame.fromString("""♜♞♝♛.♝♞♜
                                        |♟♟♟♟♟♟♟♟
                                        |........
                                        |........
                                        |........
                                        |........
                                        |♙♙♙♙♙♙♙♙
                                        |♖♘♗♕.♗♘♖""".stripMargin)
      var board = game.board

      (1 to 100).foreach { _ =>
        val movements = board.movements
        println(movements.size)
        board = board.move(movements.toList(Random.nextInt(movements.size)))
        println(board)
        println
      }
    }
  }
}
