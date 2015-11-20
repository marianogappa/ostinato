import org.scalatest.{FunSpec, ShouldMatchers}

import boardgame.core._

class BoardTest extends FunSpec with ShouldMatchers{
  describe("Board") {
    it("should correctly translate coordinates") {
      Board.fromXY(8)(0, 0) shouldBe 0
      Board.fromXY(8)(1, 0) shouldBe 1
      Board.fromXY(8)(1, 1) shouldBe 9
      Board.fromXY(10)(0, 1) shouldBe 10
      Board.fromXY(10)(1, 1) shouldBe 11
      Board.fromXY(8)(7, 7) shouldBe 63
    }
  }
}
