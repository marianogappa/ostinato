# ostinato
A modular, fully tested, very comprehensive helper library for board games, with a focus on Chess, written in Scala.

[![Build Status](https://travis-ci.org/MarianoGappa/ostinato.png)](https://travis-ci.org/MarianoGappa/ostinato)

[Basic UI demo using ChessBoardJS](http://marianogappa.github.io/ostinato-example/)

[Scaladoc](http://marianogappa.github.io/ostinato/docs)

## Status

- Chess implementation is feature complete! But AI & API have not been started yet. Some tidy up outstanding.
- Highly experimental at the moment; implementation might change drastically at any time

## Chess

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

- Support for FEN Notation (WIP)
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
```

## Use cases

- Making board game AIs
- UI API for board games (e.g. asking it "check mate?", or "which cells can I move to?" to highlight them)
- Solving/researching board games
- Inventing board games

## Short term TODO

- Fix "King threatened"-related stack overflow
- Start AI basic random implementation
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
