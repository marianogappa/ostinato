package ostinato.chess.core

import org.scalatest._

class NotationParserTest extends FunSpec with Matchers {
  import ostinato.chess.core.NotationParser.GameStep

  describe("Notation parsing") {
    it("should parse descriptive notation") {
      // https://github.com/marianogappa/ostinato/issues/1
      val descriptive =
        NotationParser.parseMatchString("""
                                            |1. P-K4 P-K4
                                            |2. N-KB3 N-QB3
                                            |3. B-B4 B-B4
                                            |4. P-QN4 BxNP
                                            |5. P-B3 B-R4
                                            |6. P-Q4 PxP
                                            |7. O-O P-Q6
                                            |8. Q-N3 Q-B3
                                            |9. P-K5 Q-N3
                                            |10. R-K1 KN-K2
                                            |11. B-R3 P-N4
                                            |12. QxP R-QN1
                                            |13. Q-R4 B-N3
                                            |14. QN-Q2 B-N2?
                                            |15. N-K4 Q-B4?
                                            |16. BxQP Q-R4
                                            |17. N-B6ch! PxN
                                            |18. PxP R-N1
                                            |19. QR-Q1! QxN
                                            |20. RxNch NxR
                                            |21. QxPch! KxQ
                                            |22. B-B5dblch K-K1
                                            |23. B-Q7ch K-B1
                                            |24. BxNmate
                                            |""".stripMargin)

      descriptive.succeeded shouldBe true
      descriptive.suceedingNotations.head shouldBe a[DescriptiveNotationRules]
      descriptive.parsedMatches.head.size shouldBe 47
    }
    it("should parse descriptive notation 2") {
      val descriptive =
        NotationParser.parseMatchString("""
                                            |1. P-K4 P-K3
                                            |2. P-Q4 P-Q4
                                            |3. N-QB3 B-N5
                                            |4. B-N5ch B-Q2
                                            |5. BxBch QxB
                                            |6. KN-K2 PxP
                                            |7. 0-0
                                            |""".stripMargin)

      descriptive.succeeded shouldBe true
      descriptive.suceedingNotations.head shouldBe a[DescriptiveNotationRules]
      descriptive.parsedMatches.head.size shouldBe 13
      descriptive.parsedMatches.head.last.maybeGameStep shouldBe
        Some(
          GameStep(CastlingAction.whiteKingside(), ChessGame.fromGridString(
            """♜♞..♚.♞♜
              |♟♟♟♛.♟♟♟
              |....♟...
              |........
              |.♝.♙♟...
              |..♘.....
              |♙♙♙.♘♙♙♙
              |♖.♗♕.♖♔.""".stripMargin, turn = BlackChessPlayer, castlingAvailable = castlingOnlyBlackAvailable,
            fullMoveNumber = 7, halfMoveClock = 1
          ).get.board)
        )
    }

    it("should parse iccf notation") {
      val iccf =
        NotationParser.parseMatchString("""
                                          |1. 5254 5756
                                          |2. 4244 4745
                                          |3. 2133 6824
                                          |4. 6125 3847
                                          |5. 2547 4847
                                          |6. 7152 4554
                                          |7. 5171""".stripMargin)

      iccf.succeeded shouldBe true
      iccf.suceedingNotations.head shouldBe a[IccfNotationRules]
      iccf.parsedMatches.head.size shouldBe 13
      iccf.parsedMatches.head.last.maybeGameStep shouldBe
        Some(
          GameStep(CastlingAction.whiteKingside(), ChessGame.fromGridString(
            """♜♞..♚.♞♜
              |♟♟♟♛.♟♟♟
              |....♟...
              |........
              |.♝.♙♟...
              |..♘.....
              |♙♙♙.♘♙♙♙
              |♖.♗♕.♖♔.""".stripMargin, turn = BlackChessPlayer, castlingAvailable = castlingOnlyBlackAvailable,
            fullMoveNumber = 7, halfMoveClock = 1
          ).get.board)
        )
    }

    it("should parse smith notation") {
      val smith =
        NotationParser.parseMatchString("""
                                          |1. e2e4  e7e6
                                          |2. d2d4  d7d5
                                          |3. b1c3  f8b4
                                          |4. f1b5  c8d7
                                          |5. b5d7b d8d7b
                                          |6. g1e2  d5e4p
                                          |7. e1g1c""".stripMargin)

      smith.succeeded shouldBe true
      smith.suceedingNotations.head shouldBe a[SmithNotationRules]
      smith.parsedMatches.head.size shouldBe 13
      smith.parsedMatches.head.last.maybeGameStep shouldBe
        Some(
          GameStep(CastlingAction.whiteKingside(), ChessGame.fromGridString(
            """♜♞..♚.♞♜
              |♟♟♟♛.♟♟♟
              |....♟...
              |........
              |.♝.♙♟...
              |..♘.....
              |♙♙♙.♘♙♙♙
              |♖.♗♕.♖♔.""".stripMargin, turn = BlackChessPlayer, castlingAvailable = castlingOnlyBlackAvailable,
            fullMoveNumber = 7, halfMoveClock = 1
          ).get.board)
        )
    }

    it("should parse coordinate notation") {
      val coordinate =
        NotationParser.parseMatchString("""
                                          |1. e2-e4 e7-e6
                                          |2. d2-d4 d7-d5
                                          |3. b1-c3 f8-b4
                                          |4. f1-b5+ c8-d7
                                          |5. b5xd7+ d8xd7
                                          |6. g1-e2 d5xe4
                                          |7. 0-0""".stripMargin)

      coordinate.succeeded shouldBe true
      coordinate.suceedingNotations.head shouldBe a[CoordinateNotationRules]
      coordinate.parsedMatches.head.size shouldBe 13
      coordinate.parsedMatches.head.last.maybeGameStep shouldBe
        Some(
          GameStep(CastlingAction.whiteKingside(), ChessGame.fromGridString(
            """♜♞..♚.♞♜
              |♟♟♟♛.♟♟♟
              |....♟...
              |........
              |.♝.♙♟...
              |..♘.....
              |♙♙♙.♘♙♙♙
              |♖.♗♕.♖♔.""".stripMargin, turn = BlackChessPlayer, castlingAvailable = castlingOnlyBlackAvailable,
            fullMoveNumber = 7, halfMoveClock = 1
          ).get.board)
        )
    }

    it("should parse algebraic notation") {
      val algebraic =
        NotationParser.parseMatchString("""e4 e6
                                          |d4 d5
                                          |Nc3 Bb4
                                          |Bb5+ Bd7
                                          |Bxd7+ Qxd7
                                          |Nge2 dxe4
                                          |0-0""".stripMargin)

      algebraic.succeeded shouldBe true
      algebraic.suceedingNotations.head shouldBe a[AlgebraicNotationRules]
      algebraic.parsedMatches.head.size shouldBe 13
      algebraic.parsedMatches.head.last.maybeGameStep shouldBe
        Some(
          GameStep(CastlingAction.whiteKingside(), ChessGame.fromGridString(
            """♜♞..♚.♞♜
              |♟♟♟♛.♟♟♟
              |....♟...
              |........
              |.♝.♙♟...
              |..♘.....
              |♙♙♙.♘♙♙♙
              |♖.♗♕.♖♔.""".stripMargin, turn = BlackChessPlayer, castlingAvailable = castlingOnlyBlackAvailable,
            fullMoveNumber = 7, halfMoveClock = 1
          ).get.board)
        )
    }

    it("should parse figurine notation") {
      val figurine =
        NotationParser.parseMatchString("""e4 e6
                                          |d4 d5
                                          |♘c3 ♝b4
                                          |♗b5+ ♝d7
                                          |♗xd7+ ♛xd7
                                          |♘ge2 dxe4
                                          |0-0""".stripMargin)

      figurine.succeeded shouldBe true
      figurine.suceedingNotations.head shouldBe a[AlgebraicNotationRules]
      figurine.parsedMatches.head.size shouldBe 13
      figurine.parsedMatches.head.last.maybeGameStep shouldBe
        Some(
          GameStep(CastlingAction.whiteKingside(), ChessGame.fromGridString(
            """♜♞..♚.♞♜
              |♟♟♟♛.♟♟♟
              |....♟...
              |........
              |.♝.♙♟...
              |..♘.....
              |♙♙♙.♘♙♙♙
              |♖.♗♕.♖♔.""".stripMargin, turn = BlackChessPlayer, castlingAvailable = castlingOnlyBlackAvailable,
            fullMoveNumber = 7, halfMoveClock = 1
          ).get.board)
        )
    }

    it("should parse pgn notation") {
      val pgn =
        NotationParser.parseMatchString("""[Event "Ostinato Testing"]
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

      pgn.succeeded shouldBe true
      pgn.suceedingNotations.head shouldBe a[AlgebraicNotationRules]
      pgn.parsedMatches.head.size shouldBe 13
      pgn.parsedMatches.head.last.maybeGameStep shouldBe
        Some(
          GameStep(CastlingAction.whiteKingside(), ChessGame.fromGridString(
            """♜♞..♚.♞♜
              |♟♟♟♛.♟♟♟
              |....♟...
              |........
              |.♝.♙♟...
              |..♘.....
              |♙♙♙.♘♙♙♙
              |♖.♗♕.♖♔.""".stripMargin, turn = BlackChessPlayer, castlingAvailable = castlingOnlyBlackAvailable,
            fullMoveNumber = 7, halfMoveClock = 1
          ).get.board)
        )
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

    val results = NotationParser.parseMatchString(s)
    results.parsedMatches.head.last.maybeGameStep.get.action shouldBe LoseAction(WhiteChessPlayer)
  }
}
