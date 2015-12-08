package boardgame.chess

import boardgame.chess.core._
import boardgame.core.XY
import org.scalatest.{FunSpec, ShouldMatchers}

class FenNotationTest extends FunSpec with ShouldMatchers {

  describe("FEN Notation") {
    it("should encode a default ChessBoard to FEN Notation") {
      ChessGame.defaultGame.board.toFen shouldBe "RNBQKBNR/PPPPPPPP/8/8/8/8/pppppppp/rnbqkbnr"
    }
    it("should encode this board") {
      ChessGame.fromString(
        """.......♚
          |........
          |♔.♟.....
          |.......♙
          |........
          |........
          |........
          |........
          |""".stripMargin).board.toFen shouldBe "7K/8/k1P5/7p/8/8/8/8"
    }
  }
}
