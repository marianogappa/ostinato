package ostinato.chess

import ostinato.chess.core._
import ostinato.core.XY
import org.scalatest.{ ShouldMatchers, FunSpec }

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

      game.blackPlayer.pieces(game.board).head.pos.toAn shouldBe AnPos('h', 8)
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

      game.whitePlayer.pieces(game.board).head.pos.toAn shouldBe AnPos('e', 4)
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
      CastlingAction(♚(XY(3, 0), WhiteChessPlayer), XY(2, 0), ♜(XY(7, 0), WhiteChessPlayer), XY(-3, 0)).toAn shouldBe "0-0-0"
      CastlingAction(♚(XY(3, 0), WhiteChessPlayer), XY(-2, 0), ♜(XY(0, 0), WhiteChessPlayer), XY(2, 0)).toAn shouldBe "0-0"
      CastlingAction(♚(XY(3, 7), BlackChessPlayer), XY(2, 0), ♜(XY(7, 7), BlackChessPlayer), XY(-3, 0)).toAn shouldBe "0-0-0"
      CastlingAction(♚(XY(3, 7), BlackChessPlayer), XY(-2, 0), ♜(XY(0, 7), BlackChessPlayer), XY(2, 0)).toAn shouldBe "0-0"
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
    it("should encode EnPassantActions into all possible notations") {
      EnPassantAction(♟(XY(1, 6), WhiteChessPlayer, -1), XY(0, -2)).allPossibleNotations shouldBe
        Set("B2-b4", "b2-b4", "♙bb4", "b4", "P-N4", "P-QN4", "PN4", "♙b2b4", "B2-B4", "b2-B4", "2224",
          "bb4", "PQN4", "♙b4", "b2b4")
      EnPassantAction(♟(XY(6, 1), WhiteChessPlayer, 1), XY(0, 2)).allPossibleNotations(ChessRules.default.copy(whitePawnDirection = 1)) shouldBe
        Set("B2-b4", "b2-b4", "♙bb4", "b4", "P-N4", "P-QN4", "PN4", "♙b2b4", "B2-B4", "b2-B4", "2224",
          "bb4", "PQN4", "♙b4", "b2b4")
    }
    it("should encode CaptureActions into all possible notations") {
      CaptureAction(♞(XY(4, 2), WhiteChessPlayer), XY(1, -2), ♜(XY(5, 0), BlackChessPlayer)).allPossibleNotations shouldBe
        Set("N:f8", "♘xf", "♘e6f", "Nef8", "e6f8kr", "QNxQR", "♘ef", "♘e:f", "N:f", "Nf", "e6xF8", "E6:f8", "Nxf",
          "E6xf8", "♘exf8", "KNxQR", "e6f8qr", "♘e6:f", "e6-F8", "Ne6:f", "Nexf8", "Ne:f", "Ne:f8", "♘e:f8", "E6-F8",
          "♘xf8", "♘e6:f8", "QNxR", "NxKR", "Nexf", "e6f8r", "♘e6xf8", "♘exf", "NxQR", "E6xF8", "e6xf8", "QNxKR", "♘:f",
          "Nxf8", "e6:F8", "♘f", "♘f8", "E6:F8", "KNxR", "5668", "Nf8", "e6-f8", "E6-f8", "Nef", "♘ef8", "e6:f8",
          "Ne6f8", "Ne6:f8", "NxR", "♘:f8", "♘e6xf", "Ne6f", "KNxKR", "♘e6f8", "Ne6xf8", "Ne6xf")
    }
    it("should encode PromoteActions into all possible notations") {
      PromoteAction(♟(XY(5, 1), WhiteChessPlayer, -1), XY(0, -1), ♜(XY(5, 0), WhiteChessPlayer)).allPossibleNotations shouldBe
        Set("F7-F8/R", "f7-F8/♖", "ff8(R)", "f7f8(♖)", "F7-F8/♖", "ff8=♖", "F7-F8(♖)", "F7-F8R", "F7-f8R", "f8♖",
          "F7-f8(R)", "ff8♖", "F7-F8=♖", "ff8(♖)", "f7-F8♖", "F7-f8/R", "ff8/R", "f7-F8/R", "f7-f8/♖", "f7-f8(♖)",
          "67682", "ff8=R", "F7-f8=♖", "ff8R", "f7f8=♖", "f8=♖", "F7-f8/♖", "f7f8(R)", "f7-f8=R", "f7-F8R", "f8/♖",
          "f7-F8=R", "f8R", "f7-f8/R", "f8=R", "f7f8/R", "F7-F8=R", "f7-f8♖", "f7f8♖", "F7-F8♖", "ff8/♖", "F7-f8(♖)",
          "f7-f8R", "F7-f8=R", "f7f8R", "f8(♖)", "f7f8/♖", "f7-F8=♖", "f7-f8(R)", "f7-f8=♖", "f8/R", "f7-F8(R)",
          "F7-F8(R)", "F7-f8♖", "f7f8=R", "f8(R)", "f7-F8(♖)")
    }
    it("should encode MoveActions into all possible notations") {
      MoveAction(♝(XY(2, 2), BlackChessPlayer), XY(4, 4)).allPossibleNotations shouldBe
        Set("Bc6g2", "c6g2", "Bg2", "B-N7", "KBKN7", "C6-g2", "QBKN7", "c6-g2", "B-KN7", "BKN7", "3672", "♝g2", "Bcg2",
          "♝cg2", "C6-G2", "KB-KN7", "KBN7", "♝c6g2", "QB-KN7", "c6-G2", "BN7", "QB-N7", "KB-N7", "QBN7")
    }
    it("should encode DrawActions into all possible notations") {
      DrawAction(BlackChessPlayer).allPossibleNotations shouldBe
        Set("½–½", "draws", "1/2-1/2")
    }
    it("should encode LoseActions into all possible notations") {
      LoseAction(BlackChessPlayer).allPossibleNotations shouldBe Set("1-0")
      LoseAction(WhiteChessPlayer).allPossibleNotations shouldBe Set("0-1")
    }
    it("should encode CastlingActions into all possible notations") {
      CastlingAction(♚(XY(4, 0), BlackChessPlayer), XY(-2, 0), ♜(XY(0, 0), BlackChessPlayer), XY(3, 0)).allPossibleNotations shouldBe
        Set("0-0-0", "e8c8C", "O-O", "castles", "Castles", "5838")
    }
    it("should encode EnPassantCaptureActions into all possible notations") {
      EnPassantCaptureAction(♟(XY(1, 3), WhiteChessPlayer, -1), XY(1, -1), ♟(XY(2, 3), BlackChessPlayer, 1)).allPossibleNotations shouldBe
        Set("B5xC6", "bc6", "b5xC6", "b5-c6", "bc6e.p.", "B5:c6", "b5c6p", "B5-c6", "B5xc6", "b5xc6", "b5-C6", "b5:C6",
          "2536", "PxP", "B5-C6", "b5:c6", "B5:C6")
    }
    it("should encode CapturePromoteActions into all possible notations") {
      CapturePromoteAction(♟(XY(2, 1), WhiteChessPlayer, -1), XY(1, -1), ♞(XY(3, 0), BlackChessPlayer), ♛(XY(3, 0), WhiteChessPlayer)).allPossibleNotations shouldBe
        Set("c:d8♕", "c7:d8=Q", "ccxd=Q", "C7:D8/Q", "C7:d8=♕", "cc7d8=♕", "c7-D8Q", "c7:d8=♕", "ccxdQ", "cd8=♕",
          "cd/♕", "cxd(♕)", "cc7xd8♕", "cc7d♕", "cc7:d(Q)", "cc7xd/♕", "PxKN=♕", "PxN/♕", "cc:d8/♕", "c7-d8=Q",
          "C7-D8/♕", "cc7xd/Q", "ccd8=♕", "C7-d8=♕", "cd=Q", "c7:d8/♕", "c7-D8=Q", "cc7xd8=Q", "C7-D8/Q", "ccxd(♕)",
          "cc:d8Q", "c7:D8=♕", "C7xD8=♕", "C7-D8Q", "c:d=♕", "C7:D8/♕", "PxKN=Q", "cc:d=♕", "c7-D8(♕)", "cxd8♕",
          "ccxd/Q", "c7xd8/Q", "c7:d8/Q", "cc7d=♕", "cc7d/Q", "cc7xd8(Q)", "cc7:dQ", "c7xd8=Q", "PxN(♕)", "c7-D8(Q)",
          "C7xD8Q", "C7:D8=♕", "cd8/♕", "PxKN/♕", "ccxd8=♕", "C7-d8/Q", "cc7dQ", "cc:d(Q)", "PxQN=♕", "C7xd8(Q)",
          "cxd(Q)", "C7xd8♕", "ccxd8/♕", "cc7d=Q", "C7-D8(Q)", "cc7:d8=♕", "ccxd8(♕)", "c:d8(♕)", "c7-d8/♕", "ccd8=Q",
          "C7:d8=Q", "cc7d8♕", "C7xD8/♕", "cd8Q", "cd(Q)", "cxd8(♕)", "c7xD8/♕", "ccd/Q", "c7:D8/♕", "cd8/Q",
          "c7:D8(Q)", "cxd/♕", "C7xD8(♕)", "C7-D8=♕", "PxN♕", "cc7:d8♕", "cc:d8/Q", "ccxd8=Q", "cd8=Q", "cxdQ",
          "C7xd8/♕", "c7xD8/Q", "ccxd8/Q", "PxQN=Q", "cc7d8=Q", "C7:D8♕", "PxQN/Q", "PxNQ", "c7-D8/Q", "cd/Q", "ccxd♕",
          "cc7d8/♕", "PxN(Q)", "cc7:d(♕)", "PxQN♕", "c:d8=♕", "PxN/Q", "c7xD8(Q)", "c7-d8(♕)", "ccdQ", "cc7:d/♕",
          "C7:d8/♕", "ccd8/Q", "PxN=Q", "ccxd8Q", "cc:d8♕", "c7-d8♕", "c7xd8♕", "C7-d8/♕", "c7-d8=♕", "PxQN(♕)",
          "c7-D8/♕", "cc:d8(♕)", "PxKNQ", "C7:d8(Q)", "ccd8(Q)", "cc:d8(Q)", "PxKN(♕)", "cc7xd(Q)", "c7:D8♕", "ccd8(♕)",
          "cd8♕", "c:d8(Q)", "c7xd8/♕", "c7d8nQ", "C7-d8♕", "cc:d♕", "c7:D8(♕)", "cc7xd8(♕)", "c7xd8(Q)", "c:d8/♕",
          "C7xD8♕", "c7-d8(Q)", "c7-d8Q", "C7:d8(♕)", "cc7:d8=Q", "C7-D8(♕)", "c7:D8=Q", "c7d8knQ", "cd♕", "ccd8/♕",
          "cc7xd8/♕", "cc7:d♕", "c7:D8Q", "c7:d8(♕)", "cxd♕", "C7xd8/Q", "c7xD8=Q", "cc7:d8Q", "C7-d8=Q", "c:d/♕",
          "C7:D8Q", "c:d(Q)", "c7-D8=♕", "c:dQ", "ccd(Q)", "ccxd8♕", "c7xD8=♕", "ccxd/♕", "cc:d8=Q", "cc7xd=♕", "cxd=♕",
          "ccd8♕", "C7xd8=Q", "cc7d/♕", "c7:d8♕", "C7xd8=♕", "cc:d8=♕", "C7-d8(♕)", "ccd8Q", "cc7:d=Q", "C7xd8Q",
          "c7xD8Q", "C7-D8♕", "cc7xd(♕)", "ccd♕", "cc7xd8Q", "c7xd8(♕)", "ccxd=♕", "cxd=Q", "cc7:d8(♕)", "PxKN/Q",
          "cc:d/♕", "cxd8=♕", "c:d(♕)", "C7-d8Q", "cc:d(♕)", "c:d8/Q", "cxd8Q", "C7-D8=Q", "C7xD8/Q", "cc7xd8/Q",
          "PxQN(Q)", "c:d=Q", "cxd8(Q)", "cc7d8/Q", "C7xD8=Q", "PxQNQ", "cc7d8(Q)", "c:d♕", "cc7xdQ", "ccd=♕",
          "cc7:d8(Q)", "c:d8=Q", "cc:d/Q", "c7xd8Q", "C7:D8=Q", "cc7d(♕)", "cd(♕)", "cd8(♕)", "c7xD8(♕)", "C7:D8(Q)",
          "C7:d8♕", "c7-D8♕", "cxd8/Q", "PxQN/♕", "cc7xd8=♕", "cc7:d=♕", "c7:D8/Q", "c:d/Q", "PxN=♕", "cxd/Q",
          "cc7xd=Q", "C7:D8(♕)", "cd=♕", "3748", "c7xD8♕", "c7xd8=♕", "C7:d8/Q", "cc7:d8/Q", "cc7xd♕", "c7-d8/Q",
          "c7:d8(Q)", "C7-d8(Q)", "cc7:d8/♕", "C7xD8(Q)", "ccd/♕", "c7:d8Q", "cxd8=Q", "cxd8/♕", "cc:d=Q", "cc7d(Q)",
          "ccd(♕)", "cc:dQ", "c:d8Q", "cc7d8(♕)", "c7d8qnQ", "C7xd8(♕)", "ccxd(Q)", "ccxd8(Q)", "ccd=Q", "cc7:d/Q",
          "PxKN(Q)", "PxKN♕", "cdQ", "cd8(Q)", "cc7d8Q", "C7:d8Q")
    }
  }

  describe("Parsing games in different notations") {
    it("should parse the same game in different notations") {
      val pgn =
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

      val algebraic =
        Notation.parseMatchString("""e4 e6
                                    |d4 d5
                                    |Nc3 Bb4
                                    |Bb5+ Bd7
                                    |Bxd7+ Qxd7
                                    |Nge2 dxe4
                                    |0-0""".stripMargin)

      val figurine =
        Notation.parseMatchString("""e4 e6
                                    |d4 d5
                                    |♘c3 ♝b4
                                    |♗b5+ ♝d7
                                    |♗xd7+ ♛xd7
                                    |♘ge2 dxe4
                                    |0-0""".stripMargin)

      val coordinate =
        Notation.parseMatchString("""
                                    |1. e2-e4 e7-e6
                                    |2. d2-d4 d7-d5
                                    |3. b1-c3 f8-b4
                                    |4. f1-b5+ c8-d7
                                    |5. b5xd7+ d8xd7
                                    |6. g1-e2 d5xe4
                                    |7. 0-0""".stripMargin)

      val descriptive =
        Notation.parseMatchString("""
                                    |1. P-K4 P-K3
                                    |2. P-Q4 P-Q4
                                    |3. N-QB3 B-N5
                                    |4. B-N5ch B-Q2
                                    |5. BxBch QxB
                                    |6. KN-K2 PxP
                                    |7. 0-0
                                    |""".stripMargin)

      val iccf =
        Notation.parseMatchString("""
                                    |1. 5254 5756
                                    |2. 4244 4745
                                    |3. 2133 6824
                                    |4. 6125 3847
                                    |5. 2547 4847
                                    |6. 7152 4554
                                    |7. 5171""".stripMargin)

      val smith =
        Notation.parseMatchString("""
                                    |1. e2e4  e7e6
                                    |2. d2d4  d7d5
                                    |3. b1c3  f8b4
                                    |4. f1b5  c8d7
                                    |5. b5d7b d8d7b
                                    |6. g1e2  d5e4p
                                    |7. e1g1c""".stripMargin)

      Set(pgn, algebraic, figurine, coordinate, descriptive, iccf, smith) foreach {
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

    it("should give proper feedback when it can't parse a whole game") {
      val parsedCoordinate =
        Notation.parseMatchString("""
                                    |1. e2-e4 e7-e6
                                    |2. d2-d4 d7-d5
                                    |3. b1-c3 INVALID_MOVE!!
                                    |4. f1-b5+ c8-d7
                                    |5. b5xd7+ d8xd7
                                    |6. g1-e2 d5xe4
                                    |7. 0-0""".stripMargin)

      parsedCoordinate match {
        case Left(states: List[(String, Option[(ChessAction, ChessBoard)])]) ⇒
          states take 5 foreach {
            case (ra: String, Some((a: ChessAction, b: ChessBoard))) ⇒
            case _ ⇒ fail
          }
          states drop 5 shouldBe
            List(
              ("INVALID_MOVE", None),
              ("f1-b5+", None),
              ("c8-d7", None),
              ("b5xd7+", None),
              ("d8xd7", None),
              ("g1-e2", None),
              ("d5xe4", None),
              ("0-0", None)
            )
        case _ ⇒ fail
      }
    }

    it("should parse final actions") {
      val s =
        """
        |[Event "1997 NAPZ/ MPP(F) M-01"]
        |[Site "ICCF"]
        |[Date "1997.??.??"]
        |[Round "?"]
        |[White "Everitt, Gordon T. (USA)"]
        |[Black "Wang, Mong Lin (SIN)"]
        |[Result "0-1"]
        |[ECO "A57"]
        |[WhiteElo "2336"]
        |[BlackElo "2428"]
        |[PlyCount "108"]
        |[EventDate "1997.??.??"]
        |
        |1. d4 Nf6 2. c4 c5 3. d5 b5 4. cxb5 a6 5. e3 axb5 6. Bxb5 Qa5+ 7. Nc3 Bb7 8.
        |Nge2 Bxd5 9. O-O Bc6 10. a4 e6 11. Ng3 d5 12. Bd2 Qd8 13. e4 d4 14. Bxc6+ Nxc6
        |15. Nb5 Be7 16. Qc2 O-O 17. Rfc1 Qb6 18. Na3 Nd7 19. Nc4 Qa6 20. a5 Nde5 21.
        |Nb6 Ra7 22. b3 Qb5 23. Nc4 Rfa8 24. Rcb1 Bd8 25. Ra4 Nd7 26. Na3 Qb7 27. b4
        |cxb4 28. Bxb4 Qa6 29. Bd2 Rc8 30. Qc4 d3 31. Qxa6 Rxa6 32. Nc4 Nc5 33. Ra3 Be7
        |34. e5 Nd4 35. f3 Ncb3 36. Raxb3 Nxb3 37. Rxb3 Rxc4 38. Rb8+ Bf8 39. Ne4 Rac6
        |40. Rd8 Rc2 41. h3 Rc8 42. Rxd3 Ra2 43. Kh2 h6 44. Rd7 g5 45. Nf6+ Kg7 46. h4
        |Rcc2 47. Ne4 gxh4 48. Kh3 Kg6 49. Rd3 Be7 50. Rd4 Bg5 51. f4 Be7 52. Rd3 Kf5
        |53. Rd4 Bc5 54. Nxc5 Rxd2 0-1
      """.stripMargin

      Notation.parseMatchString(s) match {
        case Right(s: List[(ChessAction, ChessBoard)]) ⇒ s.last._1 shouldBe LoseAction(WhiteChessPlayer)
        case _                                         ⇒ fail
      }
    }
  }
}
