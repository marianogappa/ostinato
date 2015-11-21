import org.scalatest.{FunSpec, ShouldMatchers}

import boardgame.core._

class BoardTest extends FunSpec with ShouldMatchers {
  describe("Board") {
    it("should correctly translate (10, 10) size based coordinates") {
      implicit val boardSize = BoardSize(10, 10)
      XY(0, 0).toI shouldBe 0
      XY(1, 0).toI shouldBe 1
      XY(1, 1).toI shouldBe 11
      XY(0, 1).toI shouldBe 10
      XY(9, 9).toI shouldBe 99
    }

    it("should determine if a piece exists on a board properly") {
      implicit val boardSize = BoardSize(4, 5)
      XY(0, 0).exists shouldBe true
      XY(-1, 2).exists shouldBe false
      XY(1, 2).exists shouldBe true
      XY(4, 5).exists shouldBe false
      XY(3, 4).exists shouldBe true
    }
  }

  describe("XY") {
    it("should do XY operations properly") {
      XY(0, 0) + XY(1, 1) shouldBe XY(1, 1)
      XY(2, 3) + XY(4, 5) shouldBe XY(6, 8)
      XY(4, 5) - XY(0, 0) shouldBe XY(4, 5)
      XY(0, 3) - XY(1, 2) shouldBe XY(-1, 1)
      XY(0, 0) * 2 shouldBe XY(0, 0)
      XY(1, 2) * 2 shouldBe XY(2, 4)
      XY(1, 2).sign shouldBe XY(1, 1)
      XY(1, -2).sign shouldBe XY(1, -1)
      XY(0, -2).sign shouldBe XY(0, -1)
      XY(0, -2).abs shouldBe XY(0, 2)
      XY(1, -2).abs shouldBe XY(1, 2)
      XY(0, 0).distance(XY(3, 4)) shouldBe XY(3, 4)
      XY(1, 2).distance(XY(3, 4)) shouldBe XY(2, 2)
      XY(1, 2).distance(XY(-1, -2)) shouldBe XY(2, 4)
    }
  }
}
