package ostinato.chess.core

import ostinato.core.XY
import org.scalatest._

class AlgebraicNotationSerialiserTest extends FunSpec with Matchers {
  describe("Algebraic notation for pieces") {
    it("should find black rook at h8") {
      val game = ChessGame.fromGridString(
        """.......♜
          |........
          |........
          |........
          |........
          |........
          |........
          |........""".stripMargin).get

      game.blackPlayer.pieces(game.board).head.pos.toAn shouldBe AnPos('h', 8)
    }

    it("should find black rook at a1") {
      val game = ChessGame.fromGridString(
        """........
          |........
          |........
          |........
          |........
          |........
          |........
          |♜.......""".stripMargin).get

      game.blackPlayer.pieces(game.board).head.pos.toAn shouldBe AnPos('a', 1)
    }
    it("should find white rook at d5") {
      val game = ChessGame.fromGridString(
        """........
          |........
          |........
          |...♖....
          |........
          |........
          |........
          |........""".stripMargin).get

      game.whitePlayer.pieces(game.board).head.pos.toAn shouldBe AnPos('d', 5)
    }
  }

  describe("Algebraic notation for movements") {
    it("should encode basic movements to algebraic notation") {
      MoveAction(♟(XY(0, 6), WhiteChessPlayer, -1), XY(0, -1)).toAn shouldBe "a3"
      EnPassantAction(♟(XY(0, 6), WhiteChessPlayer, -1), XY(0, -2)).toAn shouldBe "a4"
      MoveAction(♚(XY(4, 4), WhiteChessPlayer), XY(-1, -1)).toAn shouldBe "Kd5"
      MoveAction(♞(XY(5, 5), WhiteChessPlayer), XY(1, -2)).toAn shouldBe "Ng5"
      MoveAction(♝(XY(1, 3), WhiteChessPlayer), XY(3, -3)).toAn shouldBe "Be8"
      MoveAction(♜(XY(7, 7), WhiteChessPlayer), XY(-3, 0)).toAn shouldBe "Re1"
      MoveAction(♛(XY(3, 7), WhiteChessPlayer), XY(-3, -3)).toAn shouldBe "Qa4"
    }

    it("should encode castling to algebraic notation when white is on bottom") {
      CastlingAction(♚(XY(4, 7), WhiteChessPlayer), XY(2, 0), ♜(XY(7, 7), WhiteChessPlayer), XY(-2, 0)).toAn shouldBe "0-0"
      CastlingAction(♚(XY(4, 7), WhiteChessPlayer), XY(-2, 0), ♜(XY(0, 7), WhiteChessPlayer), XY(3, 0)).toAn shouldBe "0-0-0"
      CastlingAction(♚(XY(4, 0), WhiteChessPlayer), XY(2, 0), ♜(XY(7, 0), WhiteChessPlayer), XY(-2, 0)).toAn shouldBe "0-0"
      CastlingAction(♚(XY(4, 0), BlackChessPlayer), XY(-2, 0), ♜(XY(0, 0), BlackChessPlayer), XY(3, 0)).toAn shouldBe "0-0-0"
    }

    it("should encode castling properly when using sugar methods to create the actions") {
      CastlingAction.blackKingside().toAn shouldBe "0-0"
      CastlingAction.whiteKingside().toAn shouldBe "0-0"
      CastlingAction.blackQueenside().toAn shouldBe "0-0-0"
      CastlingAction.whiteQueenside().toAn shouldBe "0-0-0"
    }

    it("should encode promoting to algebraic notation") {
      PromoteAction(♟(XY(4, 1), WhiteChessPlayer, -1), XY(0, -1), ♛(XY(4, 0), WhiteChessPlayer)).toAn shouldBe "e8Q"
      PromoteAction(♟(XY(6, 6), BlackChessPlayer, 1), XY(0, 1), ♜(XY(6, 7), BlackChessPlayer)).toAn shouldBe "g1R"
    }

    it("should encode taking pieces to algebraic notation") {
      CaptureAction(♝(XY(1, 3), WhiteChessPlayer), XY(1, -1), ♞(XY(2, 2), BlackChessPlayer)).toAn shouldBe "Bxc6"
    }

    it("should encode en passant take") {
      EnPassantCaptureAction(♟(XY(1, 3), WhiteChessPlayer, -1), XY(1, -1), ♟(XY(2, 2), BlackChessPlayer, 1)).toAn shouldBe "bxc6e.p."
    }
  }

  describe("XY from Algebraic Notation") {
    it("should convert AN positions to XY") {
      ChessXY.fromAn("a1") shouldBe Some(XY(0, 7))
      ChessXY.fromAn("h8") shouldBe Some(XY(7, 0))
      ChessXY.fromAn("h1") shouldBe Some(XY(7, 7))
      ChessXY.fromAn("a8") shouldBe Some(XY(0, 0))
    }
    it("should not convert AN positions out of range") {
      ChessXY.fromAn("a0") shouldBe None
      ChessXY.fromAn("a9") shouldBe None
      ChessXY.fromAn("i1") shouldBe None
      ChessXY.fromAn("z1") shouldBe None
    }
    it("should convert ignoring case, spacing and control characters") {
      ChessXY.fromAn("A1") shouldBe Some(XY(0, 7))
      ChessXY.fromAn("A1 ") shouldBe Some(XY(0, 7))
      ChessXY.fromAn(" A1 ") shouldBe Some(XY(0, 7))
      ChessXY.fromAn("a 1") shouldBe Some(XY(0, 7))
      ChessXY.fromAn("\na \t1\r") shouldBe Some(XY(0, 7))
    }
    it("should not convert incomplete ANs or invalid characters") {
      ChessXY.fromAn("a") shouldBe None
      ChessXY.fromAn("a1♟") shouldBe None
      ChessXY.fromAn("♟♟") shouldBe None
      ChessXY.fromAn("1") shouldBe None
      ChessXY.fromAn("") shouldBe None
    }
  }
}
