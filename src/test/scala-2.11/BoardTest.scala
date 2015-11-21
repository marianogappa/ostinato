import org.scalatest.{FunSpec, ShouldMatchers}

import boardgame.core._

class BoardTest extends FunSpec with ShouldMatchers {
  describe("Board") {
    it("should correctly translate (10, 10) size based coordinates") {
      implicit val boardSize = BoardSize(10, 10)
      Point(0, 0).toPos shouldBe 0
      Point(1, 0).toPos shouldBe 1
      Point(1, 1).toPos shouldBe 11
      Point(0, 1).toPos shouldBe 10
      Point(9, 9).toPos shouldBe 99
    }

    it("should determine if a piece exists on a board properly") {
      implicit val boardSize = BoardSize(4, 5)
      Point(0, 0).exists shouldBe true
      Point(-1, 2).exists shouldBe false
      Point(1, 2).exists shouldBe true
      Point(4, 5).exists shouldBe false
      Point(3, 4).exists shouldBe true
    }
  }

  describe("XY") {
    it("should do XY operations properly") {
      Point(0, 0) + Point(1, 1) shouldBe Point(1, 1)
      Point(2, 3) + Point(4, 5) shouldBe Point(6, 8)
      Point(4, 5) - Point(0, 0) shouldBe Point(4, 5)
      Point(0, 3) - Point(1, 2) shouldBe Point(-1, 1)
      Point(0, 0) * 2 shouldBe Point(0, 0)
      Point(1, 2) * 2 shouldBe Point(2, 4)
      Point(1, 2).sign shouldBe Point(1, 1)
      Point(1, -2).sign shouldBe Point(1, -1)
      Point(0, -2).sign shouldBe Point(0, -1)
      Point(0, -2).abs shouldBe Point(0, 2)
      Point(1, -2).abs shouldBe Point(1, 2)
      Point(0, 0).distance(Point(3, 4)) shouldBe Point(3, 4)
      Point(1, 2).distance(Point(3, 4)) shouldBe Point(2, 2)
      Point(1, 2).distance(Point(-1, -2)) shouldBe Point(2, 4)
    }
  }
}
