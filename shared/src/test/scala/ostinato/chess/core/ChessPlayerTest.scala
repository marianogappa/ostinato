package ostinato.chess.core

import org.scalatest._

class ChessPlayerTest extends FunSpec with Matchers {
  describe("ChessPlayer") {
    it("should return the correct initial ranks given normal game conditions") {
      WhiteChessPlayer.initialY shouldBe 7
      BlackChessPlayer.initialY shouldBe 0
    }
  }
}
