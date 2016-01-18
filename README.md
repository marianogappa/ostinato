# ostinato
A modular, fully tested, very comprehensive helper library for board games, with a focus on Chess, written in Scala.

[![Build Status](https://travis-ci.org/MarianoGappa/ostinato.png)](https://travis-ci.org/MarianoGappa/ostinato)

[Chess Game Parser (using ChessBoardJS)](http://marianogappa.github.io/ostinato-examples/parser)

[Chess Auto-play (using ChessBoardJS)](http://marianogappa.github.io/ostinato-examples/autoplay)

[Chess Game (using ChessBoardJS)](http://marianogappa.github.io/ostinato-examples/play)

[Scaladoc](http://marianogappa.github.io/ostinato/docs)

## Features

- Feature parity with the more mature [chess.js](https://github.com/jhlywa/chess.js) minus history, PGN headers and undo, and plus a lot of useful stuff!
- Supporting the following Chess notations (with variants): PGN, Algebraic, Figurine, Coordinate, Descriptive, ICCF, Smith and FEN
- Support for importing/exporting a game state encoded in FEN notation
- Random and Basic AI available

## Technical features
- Compiled for JVM and JS => can serve as backend or in the frontend
- Fully stateless design => thread-safe & scalable; no shared mutable state & no side-effects
- `Functional design; no nulls or exception handling` x `Object Oriented design with inheritance but no downcasting`

## Status

- Chess implementation is feature complete!
- Experimental at the moment; implementation might change drastically at any time

## Chess

### Supports importing a game in any known notation without specifying the notation, with proper structured feedback when it can't
```
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
```

### Import a ChessBoard
```
// Import a chessboard by drawing it
val game = ChessGame.fromString(
        """........
          |........
          |........
          |...♜....
          |........
          |........
          |........
          |........""".stripMargin, turn = BlackChessPlayer)


// Get available actions (note that the board keeps track of turns; 0 actions for white here!)
val actions = game.board.actions


// Print them out! 
actions map board.doAction.get foreach (b => println(b + "\n"))


// -> Shows on console (outlined horizontally for brevity)
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

### Fully featured, e.g.:
- en passant
- castling
- promoting
- promoting while capturing
- check & checkmate detection with proper testing
- detection of draw due to insufficient material
- detection of draw due to stalemate
- basic AI available

### Some illustrative tests

- En Passant
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

      game.whitePlayer.pawns.head.actions(game.board).size shouldBe 1
    }
```

- Algebraic Notation (WIP)
```
CastlingAction.blackQueenside().toAn shouldBe "0-0-0"
TakeMovement(♝(XY(1, 3), WhiteChessPlayer), XY(1, -1), ♞(XY(2, 2), BlackChessPlayer)).toAn shouldBe "Bxc6"
```

- FEN Notation importing/exporting
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

- Chess AI is very slow!!
- fromString should return Option[ChessGame]; do some validation (e.g. 2 en passant pawns?; not enough cells?)
- Add `ChessGame.from` and `ChessGame.to` methods for all cases
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
