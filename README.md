![Ostinato](ostinato.jpg?v=1)
A chess library that runs on the server (Scala), on the browser (ScalaJS) and on the REPL.

[![Build Status](https://img.shields.io/travis/marianogappa/ostinato.svg)](https://travis-ci.org/marianogappa/ostinato)
[![GitHub license](https://img.shields.io/badge/license-MIT-blue.svg)](https://raw.githubusercontent.com/MarianoGappa/ostinato/master/LICENSE)

[Blogpost](http://marianogappa.github.io/software/2017/03/23/ostinato-a-chess-engine-written-in-scala-that-runs-on-the-browser-docker-and-the-repl/)

Best introduction to the library with code examples in JavaScript, Scala and in the REPL.

[Chess Game Parser (using ChessBoardJS)](http://marianogappa.github.io/ostinato-examples/parser)

Tool to paste any chess match in any known notation and browse through the moves via Chessboard. Also play from any board state against the AI, or convert to any other notation.

[Chess Game (using ChessBoardJS)](http://marianogappa.github.io/ostinato-examples/play)

Play a Chess Match against the AI. A hacker will be able to play from whatever board starting position, as white or black, configuring AI's strength and enabling debug mode.

[Chess Game Notation Converter](http://marianogappa.github.io/ostinato-examples/convert)

Convert any pasted chess match in any known notation to any other known notation.

[Chess Auto-play (using ChessBoardJS)](http://marianogappa.github.io/ostinato-examples/autoplay)

Two AI's playing each other (making random moves).

[Scaladoc](http://marianogappa.github.io/ostinato/docs)

## Features

- Feature parity with the more mature [chess.js](https://github.com/jhlywa/chess.js) minus PGN headers and undo, and plus a lot of useful stuff!
- Supporting the following Chess notations (with variants): PGN, Algebraic, Figurine, Coordinate, Descriptive, ICCF, Smith and FEN
- Support for importing/exporting a game state encoded in FEN notation
- Random and Basic AI available based on minimax with alpha-beta pruning

## Technical features
- Compiled for JVM and JS => can serve as backend or in the frontend
- Fully stateless design => thread-safe & scalable; no shared mutable state & no side-effects
- `Functional design; no nulls or exception handling` x `Object Oriented design with inheritance but no downcasting`

### Import a ChessBoard
```
$ sbt pack && scala -cp target/pack/lib/ostinato_2.12-1.0.2.jar

scala> import ostinato.chess.core._
import ostinato.chess.core._

// Import a chessboard by drawing it
scala> val game = ChessGame.fromGridString(
        """........
          |........
          |........
          |...♜....
          |........
          |........
          |........
          |........""".stripMargin, turn = BlackChessPlayer).get


// Get available actions (note that the board keeps track of turns; 0 actions for white here!)
scala> val actions = game.board.actions
actions: Set[ostinato.chess.core.ChessAction] = Set(Black's Rook moves to a5, Black's Rook moves to d2, 
Black's Rook moves to d3, ...)

// Print them out! 
scala> actions map game.board.doAction foreach (b => println(b.get + "\n"))
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

### Import game from any known notation, without specifying which one
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

### Some illustrative tests

- En Passant
```
    it("should not find en passant take move for black pawn, since king would be threatened") {
      val game = ChessGame.fromGridString(
        """....♖...
          |........
          |...↓....
          |...♙♟...
          |........
          |........
          |....♚...
          |........""".stripMargin).get

      game.whitePlayer.pawns.head.actions(game.board).size shouldBe 1
    }
```

- Algebraic Notation
```
CastlingAction.blackQueenside().toAn shouldBe "0-0-0"
TakeMovement(♝(XY(1, 3), WhiteChessPlayer), XY(1, -1), ♞(XY(2, 2), BlackChessPlayer)).toAn shouldBe "Bxc6"
```

- FEN Notation importing/exporting
```
    it("should encode this board") {
      ChessGame.fromGridString(
        """.......♔
          |........
          |♚.♙.....
          |.......♟
          |........
          |........
          |........
          |........""".stripMargin).get.board.toFen shouldBe "7K/8/k1P5/7p/8/8/8/8"
    }
    it("should decode a chess setup with black en passant in FEN Notation") {
      ChessGame.fromFen("rnbqkbnr/p1pppppp/8/1p6/8/8/PPPPPPPP/RNBQKBNR w KQkq b6 4 5") shouldBe
      Some(ChessGame.fromGridString(
      """♜♞♝♛♚♝♞♜
        |♟.♟♟♟♟♟♟
        |.↓......
        |.♟......
        |........
        |........
        |♙♙♙♙♙♙♙♙
        |♖♘♗♕♔♗♘♖""".stripMargin, turn = WhiteChessPlayer, castlingFullyAvailable, 5, 4).get)
    }    
```
