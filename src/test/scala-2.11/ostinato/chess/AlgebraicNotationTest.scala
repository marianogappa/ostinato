package ostinato.chess

import ostinato.chess.core._
import ostinato.core.XY
import org.scalatest.{ ShouldMatchers, FunSpec }

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
      EnPassantAction(♟(XY(1, 6), WhiteChessPlayer, -1), XY(0, -2)).allPossibleNotations shouldBe
        Set("B2-b4", "b2-b4", "♙bb4", "b4", "P-N4", "P-QN4", "PN4", "♙b2b4", "B2-B4", "b2-B4", "2224",
          "bb4", "PQN4", "♙b4", "b2b4")
      EnPassantAction(♟(XY(1, 1), WhiteChessPlayer, 1), XY(0, 2)).allPossibleNotations(ChessRules.default.copy(whitePawnDirection = 1)) shouldBe
        Set("B2-b4", "b2-b4", "♙bb4", "b4", "P-N4", "P-QN4", "PN4", "♙b2b4", "B2-B4", "b2-B4", "2224",
          "bb4", "PQN4", "♙b4", "b2b4")
    }
    it("should encode CaptureActions into An strings") {
      CaptureAction(♞(XY(4, 2), WhiteChessPlayer), XY(1, -2), ♜(XY(5, 0), BlackChessPlayer)).allPossibleNotations shouldBe
        Set("N:f8", "♘xf", "♘e6f", "Nef8", "e6f8kr", "QNxQR", "♘ef", "♘e:f", "N:f", "Nf", "e6xF8", "E6:f8", "Nxf",
          "E6xf8", "♘exf8", "KNxQR", "e6f8qr", "♘e6:f", "e6-F8", "Ne6:f", "Nexf8", "Ne:f", "Ne:f8", "♘e:f8", "E6-F8",
          "♘xf8", "♘e6:f8", "QNxR", "NxKR", "Nexf", "e6f8r", "♘e6xf8", "♘exf", "NxQR", "E6xF8", "e6xf8", "QNxKR", "♘:f",
          "Nxf8", "e6:F8", "♘f", "♘f8", "E6:F8", "KNxR", "5668", "Nf8", "e6-f8", "E6-f8", "Nef", "♘ef8", "e6:f8",
          "Ne6f8", "Ne6:f8", "NxR", "♘:f8", "♘e6xf", "Ne6f", "KNxKR", "♘e6f8", "Ne6xf8", "Ne6xf")
    }
    it("should encode PromoteActions into An strings") {
      PromoteAction(♟(XY(5, 1), WhiteChessPlayer, -1), XY(0, -1), ♜(XY(5, 0), WhiteChessPlayer)).allPossibleNotations shouldBe
        Set("F7-F8/R", "f7-F8/♖", "ff8(R)", "f7f8(♖)", "F7-F8/♖", "ff8=♖", "F7-F8(♖)", "F7-F8R", "F7-f8R", "f8♖",
          "F7-f8(R)", "ff8♖", "F7-F8=♖", "ff8(♖)", "f7-F8♖", "F7-f8/R", "ff8/R", "f7-F8/R", "f7-f8/♖", "f7-f8(♖)",
          "67682", "ff8=R", "F7-f8=♖", "ff8R", "f7f8=♖", "f8=♖", "F7-f8/♖", "f7f8(R)", "f7-f8=R", "f7-F8R", "f8/♖",
          "f7-F8=R", "f8R", "f7-f8/R", "f8=R", "f7f8/R", "F7-F8=R", "f7-f8♖", "f7f8♖", "F7-F8♖", "ff8/♖", "F7-f8(♖)",
          "f7-f8R", "F7-f8=R", "f7f8R", "f8(♖)", "f7f8/♖", "f7-F8=♖", "f7-f8(R)", "f7-f8=♖", "f8/R", "f7-F8(R)",
          "F7-F8(R)", "F7-f8♖", "f7f8=R", "f8(R)", "f7-F8(♖)")
    }
    it("should encode MoveActions into An strings") {
      MoveAction(♝(XY(2, 2), BlackChessPlayer), XY(4, 4)).allPossibleNotations shouldBe
        Set("Bc6g2", "c6g2", "Bg2", "B-N7", "KBKN7", "C6-g2", "QBKN7", "c6-g2", "B-KN7", "BKN7", "3672", "♝g2", "Bcg2",
          "♝cg2", "C6-G2", "KB-KN7", "KBN7", "♝c6g2", "QB-KN7", "c6-G2", "BN7", "QB-N7", "KB-N7", "QBN7")
    }
    it("should encode DrawActions into An strings") {
      DrawAction(BlackChessPlayer).allPossibleNotations shouldBe
        Set("½–½", "draws")
    }
    it("should encode CastlingActions into An strings") {
      CastlingAction(♚(XY(4, 0), BlackChessPlayer), XY(-2, 0), ♜(XY(0, 0), BlackChessPlayer), XY(3, 0)).allPossibleNotations shouldBe
        Set("0-0-0", "e8c8C", "O-O", "castles", "Castles", "5838")
    }
    it("should encode EnPassantCaptureActions into An strings") {
      EnPassantCaptureAction(♟(XY(1, 3), WhiteChessPlayer, -1), XY(1, -1), ♟(XY(2, 3), BlackChessPlayer, 1)).allPossibleNotations shouldBe
        Set("2536", "b5c6p", "bc6e.p.", "bc6")
    }
  }

  describe("Parsing games in different notations") {
    it("should parse the same game in different notations") {
      val parsedPgn =
        Notation.parseMatchString("""[Event "Ostinato Testing"]
                                    |[Site "Buenos Aires, Argentina"]
                                    |[Date "2015.??.??"]
                                    |[Round "1"]
                                    |[Result "½–½"]
                                    |[White "Fake Player 1"]
                                    |[Black "Fake Player 2"]
                                    |
                                    |1. e4 e6 2. d4 d5 3. Nc3 Bb4 4. Bb5+ Bd7 5. Bxd7+ Qxd7 6. Nge2
                                    |dxe4 7. 0-0
                                    |""".stripMargin)

      val parsedAlgebraic =
        Notation.parseMatchString("""e4 e6
                                    |d4 d5
                                    |Nc3 Bb4
                                    |Bb5+ Bd7
                                    |Bxd7+ Qxd7
                                    |Nge2 dxe4
                                    |0-0""".stripMargin)

      val parsedCoordinate =
        Notation.parseMatchString("""
                                    |1. e2-e4 e7-e6
                                    |2. d2-d4 d7-d5
                                    |3. b1-c3 f8-b4
                                    |4. f1-b5+ c8-d7
                                    |5. b5xd7+ d8xd7
                                    |6. g1-e2 d5xe4
                                    |7. 0-0""".stripMargin)

      val parsedDescriptive =
        Notation.parseMatchString("""
                                    |1. P-K4 P-K3
                                    |2. P-Q4 P-Q4
                                    |3. N-QB3 B-N5
                                    |4. B-N5ch B-Q2
                                    |5. BxBch QxB
                                    |6. KN-K2 PxP
                                    |7. 0-0
                                    |""".stripMargin)

      val parsedIccf =
        Notation.parseMatchString("""
                                    |1. 5254 5756
                                    |2. 4244 4745
                                    |3. 2133 6824
                                    |4. 6125 3847
                                    |5. 2547 4847
                                    |6. 7152 4554
                                    |7. 5171""".stripMargin)

      val parsedSmith =
        Notation.parseMatchString("""
                                    |1. e2e4  e7e6
                                    |2. d2d4  d7d5
                                    |3. b1c3  f8b4
                                    |4. f1b5  c8d7
                                    |5. b5d7b d8d7b
                                    |6. g1e2  d5e4p
                                    |7. e1g1c""".stripMargin)

      Set(parsedPgn, parsedAlgebraic, parsedCoordinate, parsedDescriptive, parsedIccf, parsedSmith) foreach {
        case Right(parsedGame) ⇒
          parsedGame.size shouldBe 13
          parsedGame.last shouldBe (CastlingAction.whiteKingside(), ChessGame.fromString(
            """♜♞..♚.♞♜
              |♟♟♟♛.♟♟♟
              |....♟...
              |........
              |.♝.♙♟...
              |..♘.....
              |♙♙♙.♘♙♙♙
              |♖.♗♕.♖♔.""".stripMargin, turn = BlackChessPlayer, castlingAvailable = castlingOnlyBlackAvailable,
            fullMoveNumber = 7, halfMoveClock = 1
          ).board)
        case _ ⇒
          fail
      }
    }
  }
}
