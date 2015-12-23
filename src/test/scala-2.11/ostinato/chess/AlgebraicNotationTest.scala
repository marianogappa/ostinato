package ostinato.chess

import ostinato.chess.core._
import ostinato.core.XY
import org.scalatest.{ShouldMatchers, FunSpec}

class AlgebraicNotationTest extends FunSpec with ShouldMatchers {
  describe("Algebraic notation for pieces") {
    it("should find black rook at a8 if white moves downwards") {
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

      game.blackPlayer.pieces(game.board).head.pos.toAn shouldBe AnPos('a', 8)
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

      game.blackPlayer.pieces(game.board).head.pos.toAn shouldBe AnPos('a', 1)
    }
    it("should find white rook at d4 if white pawn moves downwards") {
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

      game.whitePlayer.pieces(game.board).head.pos.toAn shouldBe AnPos('d', 4)
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

    it("should encode castling to algebraic notation when white is on top") {
      implicit val rules = ChessRules.default.copy(whitePawnDirection = 1)
      CastlingAction(♚(XY(4, 0), WhiteChessPlayer), XY(2, 0), ♜(XY(7, 0), WhiteChessPlayer), XY(-2, 0)).toAn shouldBe "0-0"
      CastlingAction(♚(XY(4, 0), WhiteChessPlayer), XY(-2, 0), ♜(XY(0, 0), WhiteChessPlayer), XY(3, 0)).toAn shouldBe "0-0-0"
      CastlingAction(♚(XY(4, 7), WhiteChessPlayer), XY(2, 0), ♜(XY(7, 7), WhiteChessPlayer), XY(-2, 0)).toAn shouldBe "0-0"
      CastlingAction(♚(XY(4, 7), BlackChessPlayer), XY(-2, 0), ♜(XY(0, 7), BlackChessPlayer), XY(3, 0)).toAn shouldBe "0-0-0"
    }

    it("should encode promoting to algebraic notation") {
      PromoteAction(♟(XY(4,1), WhiteChessPlayer, -1), XY(0, -1), ♛(XY(4, 0), WhiteChessPlayer)).toAn shouldBe "e8Q"
      PromoteAction(♟(XY(6,6), BlackChessPlayer, 1), XY(0, 1), ♜(XY(6, 7), BlackChessPlayer)).toAn shouldBe "g1R"
    }

    it("should encode taking pieces to algebraic notation") {
      CaptureAction(♝(XY(1, 3), WhiteChessPlayer), XY(1, -1), ♞(XY(2, 2), BlackChessPlayer)).toAn shouldBe "Bxc6"
    }

    it("should encode en passant take") {
      EnPassantCaptureAction(♟(XY(1, 3), WhiteChessPlayer, -1), XY(1, -1), ♟(XY(2, 2), BlackChessPlayer, 1)).toAn shouldBe "bxc6e.p."
    }
  }

  describe("XY from Algebraic Notation") {
    it("should convert AN positions to XY when White is on the botttom") {
      ChessXY.fromAn("a1") shouldBe Some(XY(0, 7))
      ChessXY.fromAn("h8") shouldBe Some(XY(7, 0))
      ChessXY.fromAn("h1") shouldBe Some(XY(7, 7))
      ChessXY.fromAn("a8") shouldBe Some(XY(0, 0))
    }
    it("should convert AN positions to XY when White is on top") {
      implicit val rules = ChessRules.default.copy(whitePawnDirection = 1)
      ChessXY.fromAn("a1") shouldBe Some(XY(0, 0))
      ChessXY.fromAn("h8") shouldBe Some(XY(7, 7))
      ChessXY.fromAn("h1") shouldBe Some(XY(7, 0))
      ChessXY.fromAn("a8") shouldBe Some(XY(0, 7))
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

  describe("Algebraic notations from Actions") {
    it("should encode EnPassantActions into An strings") {
      EnPassantAction(♟(XY(1, 6), WhiteChessPlayer, -1), XY(0, -2)).allPossibleAns shouldBe Set("b4", "bb4", "b2b4")
      EnPassantAction(♟(XY(1, 1), WhiteChessPlayer, 1), XY(0, 2)).allPossibleAns(ChessRules.default.copy(whitePawnDirection = 1)) shouldBe Set("b4", "bb4", "b2b4")
    }
    it("should encode CaptureActions into An strings") {
      CaptureAction(♞(XY(4, 2), WhiteChessPlayer), XY(1, -2), ♜(XY(5, 0), BlackChessPlayer)).allPossibleAns shouldBe
        Set("N:f8", "♘xf", "♘e6f", "Nef8", "♘ef8:", "♘ef", "♘e6f8:", "♘e:f", "N:f", "Nf8:", "Nf", "Nef8:", "Nxf",
          "♘exf8", "♘f:", "♘e6:f", "Ne6:f", "Nexf8", "♘e6f:", "Ne:f", "Nf:", "Ne:f8", "♘e:f8", "♘xf8", "♘e6:f8",
          "Ne6f:", "Nexf", "♘e6xf8", "♘exf", "♘:f", "Nxf8", "♘f", "Nef:", "♘f8", "♘ef:", "♘f8:", "Nf8", "Nef", "♘ef8",
          "Ne6f8", "Ne6:f8", "♘:f8", "♘e6xf", "Ne6f8:", "Ne6f", "♘e6f8", "Ne6xf8", "Ne6xf")
    }
    it("should encode PromoteActions into An strings") {
      PromoteAction(♟(XY(5, 1), WhiteChessPlayer, -1), XY(0, -1), ♜(XY(5, 0), WhiteChessPlayer)).allPossibleAns shouldBe
        Set("ff8(R)", "f7f8(♖)", "ff8=♖", "f8♖", "ff8♖", "ff8(♖)", "ff8/R", "ff8=R", "ff8R", "f7f8=♖", "f8=♖",
          "f7f8(R)", "f8/♖", "f8R", "f8=R", "f7f8/R", "f7f8♖", "ff8/♖", "f7f8R", "f8(♖)", "f7f8/♖", "f8/R", "f7f8=R",
          "f8(R)")
    }
    it("should encode MoveActions into An strings") {
      MoveAction(♝(XY(2, 2), BlackChessPlayer), XY(4, 4)).allPossibleAns shouldBe
        Set("Bc6g2", "Bg2", "♝g2", "Bcg2", "♝cg2", "♝c6g2")
    }
    it("should encode DrawActions into An strings") {
      DrawAction(BlackChessPlayer).allPossibleAns shouldBe
        Set("½–½")
    }
    it("should encode CastlingActions into An strings") {
      CastlingAction(♚(XY(4, 0), BlackChessPlayer), XY(-2, 0), ♜(XY(0, 0), BlackChessPlayer), XY(3, 0)).allPossibleAns shouldBe
        Set("0-0-0", "O-O-O")
    }
    it("should encode EnPassantCaptureActions into An strings") {
      EnPassantCaptureAction(♟(XY(1, 3), WhiteChessPlayer, -1), XY(1, -1), ♟(XY(2, 3), BlackChessPlayer, 1)).allPossibleAns shouldBe
        Set("bc6e.p.", "bc6")
    }
  }
}
