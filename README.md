# ostinato
A modular, fully tested, very comprehensive helper library for board games, with a focus on Chess, written in Scala.

[![Build Status](https://travis-ci.org/MarianoGappa/ostinato.png)](https://travis-ci.org/MarianoGappa/ostinato)

[Chess Game Parser using ChessBoardJS](http://marianogappa.github.io/ostinato-examples/parser)

[Basic UI demo using ChessBoardJS](http://marianogappa.github.io/ostinato-example/autoplay)

[Scaladoc](http://marianogappa.github.io/ostinato/docs)

## Features

- ~~Feature parity with the more mature chess.js~~ Not yet! No draw based on 3-fold repetition.
- Supporting the following Chess notations (with variants): PGN, Algebraic, Coordinate, Descriptive, ICCF, Smith, FEN.
- Support for importing/exporting a game state encoded in FEN notation.

## Technical features
- Compiled for JVM and JS => can serve as backend or in the frontend
- Fully stateless design => thread-safe & scalable; no mutable state & no side-effects
- Functional design

## Status

- ~~Chess implementation is feature complete!~~ No, it's not! Pawns can't promote and capture at the same time.
- Highly experimental at the moment; implementation might change drastically at any time

## Chess

- Supports importing a game in any known notation without specifying the notation, with proper structured feedback when it can't
```
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
```

- Import a ChessBoard
```
val game = ChessGame.fromString(
        """........
          |........
          |........
          |...♜....
          |........
          |........
          |........
          |........""".stripMargin, turn = BlackChessPlayer)
```
- Get all movements from the black Rook
```
val movements = game.board.movements
```
- Print them out! (outlined horizontally for brevity)
```
movements map board.move foreach (b => println(b + "\n"))

...♜....    ........    ........    ........    ........    ........    ........
........    ........    ........    ........    ........    ........    ........
........    ...♜....    ........    ........    ........    ........    ........
........    ........    .♜......    ♜.......    .....♜..    ........    ....♜...
........    ........    ........    ........    ........    ........    ........
........    ........    ........    ........    ........    ........    ........
........    ........    ........    ........    ........    ...♜....    ........
........    ........    ........    ........    ........    ........    ........

........    ........    ........    ........    ........    ........    ........
........    ...♜....    ........    ........    ........    ........    ........
........    ........    ........    ........    ........    ........    ........
........    ........    .......♜    ......♜.    ..♜.....    ........    ........
........    ........    ........    ........    ........    ........    ...♜....
........    ........    ........    ........    ........    ...♜....    ........
........    ........    ........    ........    ........    ........    ........
...♜....    ........    ........    ........    ........    ........    ........
```

- Fully featured; supporting en passant, castling, promoting, check & checkmate detection with proper testing
```
    it("should not find en passant take move for black pawn, since king would be threatened") {
      implicit val rules = ChessRules.default.copy(whitePawnDirection = 1)
      val game = ChessGame.fromString(
        """....♖...
          |........
          |...↓....
          |...♙♟...
          |........
          |........
          |....♚...
          |........""".stripMargin)

      game.whitePlayer.pawns.head.movements(game.board).size shouldBe 1
    }
```

- Support for Algebraic Notation (WIP)
```
TakeMovement(♝(XY(1, 3), WhiteChessPlayer), XY(1, -1), ♞(XY(2, 2), BlackChessPlayer)).toAn shouldBe "Bxc6"
```

- Support for FEN Notation importing/exporting
```
    it("should encode this board") {
      ChessGame.fromString(
        """.......♔
          |........
          |♚.♙.....
          |.......♟
          |........
          |........
          |........
          |........
          |""".stripMargin).board.toFen shouldBe "7K/8/k1P5/7p/8/8/8/8"
    }
    it("should decode a chess setup with black en passant in FEN Notation") {
      ChessGame.fromFen("rnbqkbnr/p1pppppp/8/1p6/8/8/PPPPPPPP/RNBQKBNR w KQkq b6 4 5") shouldBe
      Some(ChessGame.fromString(
      """♜♞♝♛♚♝♞♜
        |♟.♟♟♟♟♟♟
        |.↓......
        |.♟......
        |........
        |........
        |♙♙♙♙♙♙♙♙
        |♖♘♗♕♔♗♘♖
        |""".stripMargin, turn = WhiteChessPlayer, castlingFullyAvailable, 5, 4))
    }    
```

## Use cases

- Making board game AIs
- UI API for board games (e.g. asking it "check mate?", or "which cells can I move to?" to highlight them)
- Solving/researching board games
- Inventing board games

## Short term TODO

- Complete support for all known notations
- Pawn should be able to capture and promote in one action!
- Research repositories

## Long term TODO

- Implement PegSolitaireGame as proof of concept
- Solve PegSolitaireGame as proof of concept
- Implement Chess Game as proof of concept
- Implement partial/complete algebraic chess notation to Game: can display and practice from there
- Invent awesome game using this library

## Anyway; why "ostinato"?

The author is a musician of sorts and deeply loves music.

"Ostinato" stands for something like: "a short melody or rhythm that is repeated by the same voice or instrument during a musical composition"; although it comes from "stubborn" (italian). The author is a very stubborn (がんこ) man of italian descent who came up with this idea of making a board game based on a chess board and chess pieces but with different rules, as a team project for his company's FedEx day. He persisted on his unpopular idea and kept repeating how great it was even though no one agreed. So.
