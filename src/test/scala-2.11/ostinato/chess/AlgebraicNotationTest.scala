package ostinato.chess

import ostinato.chess.core._
import ostinato.core.XY
import org.scalatest.{ShouldMatchers, FunSpec}

class AlgebraicNotationTest extends FunSpec with ShouldMatchers {
  describe("Algebraic notation for pieces") {
    it("should find black rook at h8 if white moves downwards") {
      implicit val rules = ChessRules.default.copy(whitePawnDirection = 1)
      val game = ChessGame.fromString(
        """........
          |........
          |........
          |........
          |........
          |........
          |........
          |♜.......""".stripMargin)

      game.blackPlayer.pieces(game.board).head.pos.toAn shouldBe An('h', 8)
    }
    it("should find black rook at a1 if white pawn moves upwards") {
      val game = ChessGame.fromString(
        """........
          |........
          |........
          |........
          |........
          |........
          |........
          |♜.......""".stripMargin)

      game.blackPlayer.pieces(game.board).head.pos.toAn shouldBe An('a', 1)
    }
    it("should find white rook at e4 if white pawn moves downwards") {
      implicit val rules = ChessRules.default.copy(whitePawnDirection = 1)
      val game = ChessGame.fromString(
        """........
          |........
          |........
          |...♖....
          |........
          |........
          |........
          |........""".stripMargin)

      game.whitePlayer.pieces(game.board).head.pos.toAn shouldBe An('e', 4)
    }
  }

  describe("Algebraic notation for movements") {
    it("should encode basic movements to algebraic notation") {
      MoveMovement(♟(XY(0, 6), WhiteChessPlayer, -1), XY(0, -1)).toAn shouldBe "a3"
      EnPassantMovement(♟(XY(0, 6), WhiteChessPlayer, -1), XY(0, -2)).toAn shouldBe "a4"
      MoveMovement(♚(XY(4, 4), WhiteChessPlayer), XY(-1, -1)).toAn shouldBe "Kd5"
      MoveMovement(♞(XY(5, 5), WhiteChessPlayer), XY(1, -2)).toAn shouldBe "Ng5"
      MoveMovement(♝(XY(1, 3), WhiteChessPlayer), XY(3, -3)).toAn shouldBe "Be8"
      MoveMovement(♜(XY(7, 7), WhiteChessPlayer), XY(-3, 0)).toAn shouldBe "Re1"
      MoveMovement(♛(XY(3, 7), WhiteChessPlayer), XY(-3, -3)).toAn shouldBe "Qa4"
    }

    it("should encode castling to algebraic notation when white is on bottom") {
      CastlingMovement(♚(XY(4, 7), WhiteChessPlayer), XY(2, 0), ♜(XY(7, 7), WhiteChessPlayer), XY(-2, 0)).toAn shouldBe "0-0"
      CastlingMovement(♚(XY(4, 7), WhiteChessPlayer), XY(-2, 0), ♜(XY(0, 7), WhiteChessPlayer), XY(3, 0)).toAn shouldBe "0-0-0"
      CastlingMovement(♚(XY(4, 0), WhiteChessPlayer), XY(2, 0), ♜(XY(7, 0), WhiteChessPlayer), XY(-2, 0)).toAn shouldBe "0-0"
      CastlingMovement(♚(XY(4, 0), BlackChessPlayer), XY(-2, 0), ♜(XY(0, 0), BlackChessPlayer), XY(3, 0)).toAn shouldBe "0-0-0"
    }

    it("should encode castling to algebraic notation when white is on top") {
      implicit val rules = ChessRules.default.copy(whitePawnDirection = 1)
      CastlingMovement(♚(XY(4, 0), WhiteChessPlayer), XY(2, 0), ♜(XY(7, 0), WhiteChessPlayer), XY(-2, 0)).toAn shouldBe "0-0"
      CastlingMovement(♚(XY(4, 0), WhiteChessPlayer), XY(-2, 0), ♜(XY(0, 0), WhiteChessPlayer), XY(3, 0)).toAn shouldBe "0-0-0"
      CastlingMovement(♚(XY(4, 7), WhiteChessPlayer), XY(2, 0), ♜(XY(7, 7), WhiteChessPlayer), XY(-2, 0)).toAn shouldBe "0-0"
      CastlingMovement(♚(XY(4, 7), BlackChessPlayer), XY(-2, 0), ♜(XY(0, 7), BlackChessPlayer), XY(3, 0)).toAn shouldBe "0-0-0"
    }

    it("should encode promoting to algebraic notation") {
      PromoteMovement(♟(XY(4,1), WhiteChessPlayer, -1), XY(0, -1), ♛(XY(4, 0), WhiteChessPlayer)).toAn shouldBe "e8Q"
      PromoteMovement(♟(XY(6,6), BlackChessPlayer, 1), XY(0, 1), ♜(XY(6, 7), BlackChessPlayer)).toAn shouldBe "g1R"
    }

    it("should encode taking pieces to algebraic notation") {
      TakeMovement(♝(XY(1, 3), WhiteChessPlayer), XY(1, -1), ♞(XY(2, 2), BlackChessPlayer)).toAn shouldBe "Bxc6"
    }

    it("should encode en passant take") {
      EnPassantTakeMovement(♟(XY(1, 3), WhiteChessPlayer, -1), XY(1, -1), ♟(XY(2, 2), BlackChessPlayer, 1)).toAn shouldBe "bxc6e.p."
    }
  }
}
