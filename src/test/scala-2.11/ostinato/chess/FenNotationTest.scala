package ostinato.chess

import ostinato.chess.core._
import ostinato.core.XY
import org.scalatest.{FunSpec, ShouldMatchers}

class FenNotationTest extends FunSpec with ShouldMatchers {

  describe("FEN Notation") {
    it("should encode a default ChessBoard to FEN Notation") {
      ChessGame.defaultGame.board.toFen shouldBe "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
    }
    it("should encode this board") {
      ChessGame.fromString(
        """.......♔
          |........
          |♚.♙.....
          |.......♟
          |........
          |........
          |........
          |........
          |""".stripMargin).board.toFen shouldBe "7K/8/k1P5/7p/8/8/8/8"
    }
  }
}
