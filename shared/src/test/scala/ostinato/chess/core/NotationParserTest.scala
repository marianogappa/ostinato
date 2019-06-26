package ostinato.chess.core

import org.scalatest._
import ostinato.chess.core.NotationParser.PreParseInsights

class NotationParserTest extends FunSpec with Matchers {

  import ostinato.chess.core.NotationParser.GameStep

  describe("Notation parsing") {
    val algebraicSerialiser = AlgebraicNotationActionSerialiser(
      AlgebraicNotationRules(
        lowerCaseLetters = true,
        figurine = false,
        distinguishCaptures = true,
        colonForCaptures = false,
        castlingNotation = "zeroes",
        hashForCheckmate = true,
        noFromPosForPawns = true,
        checkSymbol = CheckSymbol.PLUS,
        noFromPosOnCapturesExceptPawns = true
      )
    )

    def descriptiveToAlgebraic(descriptive: NotationParser.ParseResultsProxy) = descriptive.parsedMatches.head
      .flatMap(parseStep ⇒
        parseStep.maybeGameStep.map(
          gameStep ⇒
            algebraicSerialiser
              .serialiseAction(gameStep.action, parseStep.preParseInsights)
              .head
              ._1
        )
      )
      .toArray

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
          GameStep(
            CastlingAction.whiteKingside(),
            ChessGame
              .fromGridString(
                """♜♞..♚.♞♜
              |♟♟♟♛.♟♟♟
              |....♟...
              |........
              |.♝.♙♟...
              |..♘.....
              |♙♙♙.♘♙♙♙
              |♖.♗♕.♖♔.""".stripMargin,
                turn = BlackChessPlayer,
                castlingAvailable = castlingOnlyBlackAvailable,
                fullMoveNumber = 7,
                halfMoveClock = 1
              )
              .get
              .board
          )
        )
    }
    it("should parse descriptive notation with initial board, using N for knight") {
      val descriptive =
        NotationParser.parseMatchString("""
            |1. KxP K-N6
            |2. K-B6 K-B5
            |3. K-Q6 K-Q5
            |4. P-B4 K-K5
            |""".stripMargin, ChessGame.fromFen("8/1pK5/8/8/8/8/1k3P2/8 w - - 0 1").get.board)

      descriptive.succeeded shouldBe true
      descriptive.suceedingNotations.head shouldBe a[DescriptiveNotationRules]
      descriptive.parsedMatches.head.size shouldBe 8
    }
    it("should parse descriptive notation with initial board, using Kt for knight") {
      val descriptive =
        NotationParser.parseMatchString("""
            |1. KxP K-Kt6
            |2. K-B6 K-B5
            |3. K-Q6 K-Q5
            |4. P-B4 K-K5
            |""".stripMargin, ChessGame.fromFen("8/1pK5/8/8/8/8/1k3P2/8 w - - 0 1").get.board)

      descriptive.succeeded shouldBe true
      descriptive.suceedingNotations.head shouldBe a[DescriptiveNotationRules]
      descriptive.parsedMatches.head.size shouldBe 8
    }

    // This is a very good debugging snippet: gives you calculated possible next moves in the given notation
    //      val as = descriptive.parsedMatches.head.last.maybeGameStep.get.board.actions
    //      val s = DescriptiveNotationActionSerialiser(DescriptiveNotationRules(false, false, false, "castles"))
    //      val b: Set[(String, (ChessAction, NotationRules))] = as.flatMap(s.serialiseAction)
    //      b.map(_._1) foreach println

    it("should parse descriptive notation with initial board 2") {
      val descriptive =
        NotationParser.parseMatchString("""
          |1. K-Q6!      K-R6
          |2. K-B5       K-R5
          |3. P-B4       P-Kt4
          |4. P-B5       P-Kt5
          |5. K-B4!      P-Kt6
          |6. K-B3!      K-R6
          |7. P-B6       P-Kt7
          |8. P-B7       P-Kt8=Q
          |9. P-B8(Q)ch  K-R5
          |10. Q-R8ch    K-Kt4
          |11. Q-Kt7ch   1-0
          |""".stripMargin, ChessGame.fromFen("8/1pK5/8/8/8/8/k4P2/8 w - - 0 1").get.board)

      descriptive.succeeded shouldBe true
      descriptive.suceedingNotations.head shouldBe a[DescriptiveNotationRules]
      descriptive.parsedMatches.head.size shouldBe 22

    }

    it("should parse descriptive notation Irving Chernev Practical Chess Endings Game 1") {
      val actual = descriptiveToAlgebraic(
        NotationParser.parseMatchString("""
          |1. P-R4 K-B5
          |2. P-R5 K-Q4
          |3. P-R6 K-K3
          |4. P-R7 K-B2
          |5. P-R8(Q) Resigns
          |""".stripMargin, ChessGame.fromFen("8/8/8/8/8/1k5P/8/2K5 w - - 0 1").get.board)
      )

      actual shouldBe Array(
        "h4",
        "Kc4",
        "h5",
        "Kd5",
        "h6",
        "Ke6",
        "h7",
        "Kf7",
        "h8=Q",
        "1-0" // could maintain Resigns
      )
    }

    it("should parse descriptive notation Irving Chernev Practical Chess Endings Game 2") {
      val actual = descriptiveToAlgebraic(
        NotationParser.parseMatchString("""
          |1. K-B5!  K-K6
          |2. K-K5!  K-Q6
          |3. K-Q5!  K-B6
          |4. K-B5!  K-Q6
          |5. P-R4    K-B6
          |6. P-R5    K-Kt6
          |7. P-R6    K-R5
          |8. P-R7    K-R4
          |9. P-R8(Q) mate
          |""".stripMargin, ChessGame.fromFen("8/8/8/6K1/8/5k2/P7/8 w - - 0 1").get.board)
      )

      actual shouldBe Array(
        "Kf5!",
        "Ke3",
        "Ke5!",
        "Kd3",
        "Kd5!",
        "Kc3",
        "Kc5!",
        "Kd3",
        "a4",
        "Kc3",
        "a5",
        "Kb3",
        "a6",
        "Ka4",
        "a7",
        "Ka5",
        "a8=Q#"
      )
    }

    it("should parse descriptive notation Irving Chernev Practical Chess Endings Game 3") {
      val actual = descriptiveToAlgebraic(
        NotationParser.parseMatchString("""
          |1. K-K6! K-Q1
          |2. K-B7 K-Q2
          |3. P-K6ch K-Q1
          |4. P-K7ch K-Q2
          |5. P-K8(Q)ch
          |""".stripMargin, ChessGame.fromFen("4k3/8/3K4/4P3/8/8/8/8 w - - 0 1").get.board)
      )

      actual shouldBe Array(
        "Ke6!",
        "Kd8",
        "Kf7",
        "Kd7",
        "e6+",
        "Kd8",
        "e7+",
        "Kd7",
        "e8=Q+"
      )
    }

    it("should parse descriptive notation Irving Chernev Practical Chess Endings Game 5") {
      val actual = descriptiveToAlgebraic(
        NotationParser.parseMatchString("""
          |1. K-Q5 K-Q2
          |2. K-B5 K-Q1
          |3. K-Q6! K-B1
          |4. K-B6 K-Kt1
          |5. P-Kt7 K-R2
          |6. K-B7
          |""".stripMargin, ChessGame.fromFen("3k4/8/1P6/8/4K3/8/8/8 w - - 0 1").get.board)
      )

      actual shouldBe Array(
        "Kd5",
        "Kd7",
        "Kc5",
        "Kd8",
        "Kd6!",
        "Kc8",
        "Kc6",
        "Kb8",
        "b7",
        "Ka7",
        "Kc7"
      )
    }

    it("should parse descriptive notation Irving Chernev Practical Chess Endings Game 6") {
      val actual = descriptiveToAlgebraic(
        NotationParser.parseMatchString("""
          |1. K-Kt6! K-B1
          |2. K-B6 K-K1
          |3. K-K6 K-Q1
          |4. K-Q6 K-B1
          |5. K-B6 K-Kt1
          |6. K-Q7 K-Kt2
          |7. P-B5 K-Kt1
          |8. P-B6 K-R2
          |9. P-B7
          |""".stripMargin, ChessGame.fromFen("6k1/8/7K/8/2P5/8/8/8 w - - 0 1").get.board)
      )

      actual shouldBe Array(
        "Kg6!",
        "Kf8",
        "Kf6",
        "Ke8",
        "Ke6",
        "Kd8",
        "Kd6",
        "Kc8",
        "Kc6",
        "Kb8",
        "Kd7",
        "Kb7",
        "c5",
        "Kb8",
        "c6",
        "Ka7",
        "c7"
      )
    }

    it("should parse descriptive notation Irving Chernev Practical Chess Endings Game 7") {
      val actual = descriptiveToAlgebraic(
        NotationParser.parseMatchString("""
          |1. K-B7! K-R1
          |2. K-Kt6! K-Kt1
          |3. K-R6 K-R1
          |4. P-Kt6 K-Kt1
          |5. P-Kt7 K-B2
          |6. K-R7 K-K2
          |7. P-Kt8(Q)
          |""".stripMargin, ChessGame.fromFen("8/7k/5K2/6P1/8/8/8/8 w - - 0 1").get.board)
      )

      actual shouldBe Array(
        "Kf7!",
        "Kh8",
        "Kg6!",
        "Kg8",
        "Kh6",
        "Kh8",
        "g6",
        "Kg8",
        "g7",
        "Kf7",
        "Kh7",
        "Ke7",
        "g8=Q"
      )
    }

    it("should parse descriptive notation Irving Chernev Practical Chess Endings Game 8") {
      val actual = descriptiveToAlgebraic(
        NotationParser.parseMatchString("""
          |1. K-K4 K-K3
          |2. P-K3! K-Q3
          |3. K-B5 K-K2
          |4. K-K5 K-Q2
          |5. K-B6 K-K1
          |6. K-K6 K-B1
          |7. P-K4 K-K1
          |8. P-K5 K-B1
          |9. K-Q7 K-B2
          |10. P-K6ch K-B1
          |11. P-K7ch
          |""".stripMargin, ChessGame.fromFen("8/8/5k2/8/5K2/8/4P3/8 w - - 0 1").get.board)
      )

      actual shouldBe Array(
        "Ke4",
        "Ke6",
        "e3!",
        "Kd6",
        "Kf5",
        "Ke7",
        "Ke5",
        "Kd7",
        "Kf6",
        "Ke8",
        "Ke6",
        "Kf8",
        "e4",
        "Ke8",
        "e5",
        "Kf8",
        "Kd7",
        "Kf7",
        "e6+",
        "Kf8",
        "e7+"
      )
    }

    it("should parse descriptive notation Irving Chernev Practical Chess Endings Game 9") {
      val actual = descriptiveToAlgebraic(
        NotationParser.parseMatchString("""
          |1. K-B2 K-Kt3
          |2. K-K3 K-B4
          |3. K-Q4! K-K3
          |4. K-B5 K-Q2
          |5. K-Q5 K-K2
          |6. K-B6 K-K3
          |7. P-Q4 K-K2
          |8. P-Q5 K-Q1
          |9. K-Q6 Resigns
          |""".stripMargin, ChessGame.fromFen("8/7k/8/8/8/3P4/8/6K1 w - - 0 1").get.board)
      )

      actual shouldBe Array(
        "Kf2",
        "Kg6",
        "Ke3",
        "Kf5",
        "Kd4!",
        "Ke6",
        "Kc5",
        "Kd7",
        "Kd5",
        "Ke7",
        "Kc6",
        "Ke6",
        "d4",
        "Ke7",
        "d5",
        "Kd8",
        "Kd6",
        "1-0" // Could maintain "resigns"
      )
    }

    it("should parse descriptive notation Irving Chernev Practical Chess Endings Game 12") {
      val actual = descriptiveToAlgebraic(
        NotationParser.parseMatchString("""
          |1. K-B4 K-Kt2
          |2. K-B5 K-R1
          |3. K-Kt5 K-Kt2
          |4. P-R8(Q)ch! KxQ
          |5. K-B6 K-Kt1
          |6. P-Kt7 K-R2
          |7. K-B7 Resigns
          |""".stripMargin, ChessGame.fromFen("7k/7P/6P1/8/8/6K1/8/8 w - - 0 1").get.board)
      )

      actual shouldBe Array(
        "Kf4",
        "Kg7",
        "Kf5",
        "Kh8",
        "Kg5",
        "Kg7",
        "h8=Q+!",
        "Kxh8",
        "Kf6",
        "Kg8",
        "g7",
        "Kh7",
        "Kf7",
        "1-0" // Could maintain "resigns"
      )
    }

    it("should parse descriptive notation Irving Chernev Practical Chess Endings Game 13") {
      val actual = descriptiveToAlgebraic(
        NotationParser.parseMatchString("""
          |1. P-Kt7 K-R2
          |2. P-Kt8(Q)ch! KxQ
          |3. K-Kt6 K-R1
          |4. K-B7 K-R2
          |5. P-Kt6ch K-R1
          |6. P-Kt7ch  Resigns
          |""".stripMargin, ChessGame.fromFen("6k1/8/5KP1/6P1/8/8/8/8 w - - 0 1").get.board)
      )

      actual shouldBe Array(
        "g7",
        "Kh7",
        "g8=Q+!",
        "Kxg8",
        "Kg6",
        "Kh8",
        "Kf7",
        "Kh7",
        "g6+",
        "Kh8",
        "g7+",
        "1-0" // Could maintain "resigns"
      )
    }

    it("should parse descriptive notation Irving Chernev Practical Chess Endings Game 24") {
      val actual = descriptiveToAlgebraic(
        NotationParser.parseMatchString("""
          |1. K-B5! P-Kt4
          |2. P-Kt4 P-Kt5
          |3. K-Q4 P-Kt6
          |4. K-K3 K-Kt4
          |5. P-Kt5 K-Kt5
          |6. P-Kt6 K-R6
          |7. P-Kt7 P-Kt7
          |8. K-B2 K-R7
          |9. P-Kt8(Q)ch
          |""".stripMargin, ChessGame.fromFen("8/6p1/7k/8/1K6/8/1P6/8 w - - 0 1").get.board)
      )

      actual shouldBe Array(
        "Kc5!", "g5",
        "b4", "g4",
        "Kd4", "g3",
        "Ke3", "Kg5",
        "b5", "Kg4",
        "b6", "Kh3",
        "b7", "g2",
        "Kf2", "Kh2",
        "b8=Q+"
      )
    }

    it("should parse descriptive notation Irving Chernev Practical Chess Endings Game 85") {
      val actual = descriptiveToAlgebraic(
        NotationParser.parseMatchString("""
                                          |1.   P-KB6! PxP
                                          |2.   KxP     K-Kt4
                                          |3.   P-R4    PxPe.p.
                                          |4.   PxP      K-B4
                                          |5.   P-R4    K-K4
                                          |6.   P-Q6!  PxP
                                          |7.   P-B6!   PxP
                                          |8.   P-R5
          |""".stripMargin, ChessGame.fromFen("8/2pp2pp/8/2PP1P2/1p5k/8/PP3p2/5K2 w - - 0 1").get.board)
      )

      actual shouldBe Array(
        "f6!", "gxf6", "Kxf2", "Kg5", "a4", "bxa3e.p.", "bxa3", "Kf5", "a4", "Ke5", "d6!", "cxd6", "c6!", "dxc6", "a5"
      )
    }

    it("should parse descriptive notation Irving Chernev Practical Chess Endings Game 294") {
      val actual = descriptiveToAlgebraic(
        NotationParser.parseMatchString("""
                                          |1. P-Kt7!  K-B2
                                          |2. R-R8  QxKtP
                                          |3. R-B8ch! KxRch
                                          |4. P-Q7dis.ch  K-B2
                                          |5. P-Q8(N)ch!  K-K1ch
                                          |6. KtxQ
          |""".stripMargin, ChessGame.fromFen("1q3k2/7K/1P1P1p2/R2P4/8/B5p1/8/8 w - - 0 1").get.board)
      )

      actual shouldBe Array(
        "b7!",
        "Kf7",
        "Ra8",
        "Qxb7",
        "Rf8+!",
        "Kxf8+",
        "d7+",
        "Kf7",
        "d8=N+!",
        "Ke8+",
        "Nxb7"
      )
    }

    it("should parse descriptive notation Irving Chernev Practical Chess Endings Game 296") {
      val actual = descriptiveToAlgebraic(
        NotationParser.parseMatchString("""
                                          |1. R(R5)-QR5 K-B3
                                          |2. K-B8  K-Q3
                                          |3. K-Q8  K-K3
                                          |4. R(R8)-R6ch  K-B2
                                          |5. R-B5ch  K-Kt2
                                          |6. R-Kt5ch K-B2
                                          |7. R(Kt)-Kt6 P-Kt8(Q)
                                          |8. R(R6)-B6
          |""".stripMargin, ChessGame.fromFen("RK6/8/1k6/7R/8/8/pp6/8 w - - 0 1").get.board)
      )

      actual shouldBe Array(
        "Ra5",
        "Kc6",
        "Kc8",
        "Kd6",
        "Kd8",
        "Ke6",
        "Ra6+",
        "Kf7",
        "Rf5+",
        "Kg7",
        "Rg5+",
        "Kf7",
        "Rg6",
        "b1=Q",
        "Rc6"
      )
    }

    it("should prepareMatchString properly") {
      val descriptiveSerialiser = DescriptiveNotationActionSerialiser(
        DescriptiveNotationRules(
          omitDash = false,
          numericalRankBeforeFile = false,
          omitFirstRank = false,
          castlingNotation = "zeroes"
        )
      )
      val noInsights = PreParseInsights()
      descriptiveSerialiser.prepareMatchString(
        """
          |1. K-B5!  K-K6
          |2. K-K5!  K-Q6
          |3. K-Q5!  K-B6
          |4. K-B5!  K-Q6
          |5. P-R4    K-B6
          |6. P-R5    K-Kt6
          |7. P-R6    K-R5
          |8. P-R7    K-R4
          |9. P-R8(Q) mate
          |""".stripMargin) shouldBe List(
        ("K-B5", noInsights.copy(good = true)),
        ("K-K6", noInsights),
        ("K-K5", noInsights.copy(good = true)),
        ("K-Q6", noInsights),
        ("K-Q5", noInsights.copy(good = true)),
        ("K-B6", noInsights),
        ("K-B5", noInsights.copy(good = true)),
        ("K-Q6", noInsights),
        ("P-R4", noInsights),
        ("K-B6", noInsights),
        ("P-R5", noInsights),
        ("K-KT6", noInsights),
        ("P-R6", noInsights),
        ("K-R5", noInsights),
        ("P-R7", noInsights),
        ("K-R4", noInsights),
        ("P-R8(Q)MATE", noInsights)
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
          GameStep(
            CastlingAction.whiteKingside(),
            ChessGame
              .fromGridString(
                """♜♞..♚.♞♜
              |♟♟♟♛.♟♟♟
              |....♟...
              |........
              |.♝.♙♟...
              |..♘.....
              |♙♙♙.♘♙♙♙
              |♖.♗♕.♖♔.""".stripMargin,
                turn = BlackChessPlayer,
                castlingAvailable = castlingOnlyBlackAvailable,
                fullMoveNumber = 7,
                halfMoveClock = 1
              )
              .get
              .board
          )
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
          GameStep(
            CastlingAction.whiteKingside(),
            ChessGame
              .fromGridString(
                """♜♞..♚.♞♜
              |♟♟♟♛.♟♟♟
              |....♟...
              |........
              |.♝.♙♟...
              |..♘.....
              |♙♙♙.♘♙♙♙
              |♖.♗♕.♖♔.""".stripMargin,
                turn = BlackChessPlayer,
                castlingAvailable = castlingOnlyBlackAvailable,
                fullMoveNumber = 7,
                halfMoveClock = 1
              )
              .get
              .board
          )
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
          GameStep(
            CastlingAction.whiteKingside(),
            ChessGame
              .fromGridString(
                """♜♞..♚.♞♜
              |♟♟♟♛.♟♟♟
              |....♟...
              |........
              |.♝.♙♟...
              |..♘.....
              |♙♙♙.♘♙♙♙
              |♖.♗♕.♖♔.""".stripMargin,
                turn = BlackChessPlayer,
                castlingAvailable = castlingOnlyBlackAvailable,
                fullMoveNumber = 7,
                halfMoveClock = 1
              )
              .get
              .board
          )
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
          GameStep(
            CastlingAction.whiteKingside(),
            ChessGame
              .fromGridString(
                """♜♞..♚.♞♜
              |♟♟♟♛.♟♟♟
              |....♟...
              |........
              |.♝.♙♟...
              |..♘.....
              |♙♙♙.♘♙♙♙
              |♖.♗♕.♖♔.""".stripMargin,
                turn = BlackChessPlayer,
                castlingAvailable = castlingOnlyBlackAvailable,
                fullMoveNumber = 7,
                halfMoveClock = 1
              )
              .get
              .board
          )
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
          GameStep(
            CastlingAction.whiteKingside(),
            ChessGame
              .fromGridString(
                """♜♞..♚.♞♜
              |♟♟♟♛.♟♟♟
              |....♟...
              |........
              |.♝.♙♟...
              |..♘.....
              |♙♙♙.♘♙♙♙
              |♖.♗♕.♖♔.""".stripMargin,
                turn = BlackChessPlayer,
                castlingAvailable = castlingOnlyBlackAvailable,
                fullMoveNumber = 7,
                halfMoveClock = 1
              )
              .get
              .board
          )
        )
    }

    it("should parse pgn notation") {
      val pgn =
        NotationParser.parseMatchString(
          """[Event "Ostinato Testing"]
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
          GameStep(
            CastlingAction.whiteKingside(),
            ChessGame
              .fromGridString(
                """♜♞..♚.♞♜
              |♟♟♟♛.♟♟♟
              |....♟...
              |........
              |.♝.♙♟...
              |..♘.....
              |♙♙♙.♘♙♙♙
              |♖.♗♕.♖♔.""".stripMargin,
                turn = BlackChessPlayer,
                castlingAvailable = castlingOnlyBlackAvailable,
                fullMoveNumber = 7,
                halfMoveClock = 1
              )
              .get
              .board
          )
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
    results.parsedMatches.head.last.maybeGameStep.get.action shouldBe LoseAction(
      WhiteChessPlayer)
  }
}
