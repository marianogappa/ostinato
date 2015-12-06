# board-game-helper
A modular, fully tested, very comprehensive helper library for board games, with a focus on Chess, written in Scala.

[![Build Status](https://travis-ci.org/MarianoGappa/board-game-helper.png)](https://travis-ci.org/MarianoGappa/board-game-helper)

## Status

- Chess implementation is feature complete! But AI, UI & API have not been started yet. Some tidy up outstanding.
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
          |........""".stripMargin)
```
- Get all movements from the black Rook
```
val board = game.board
implicit val rules = game.rules

val movements = game.blackPlayer.pieces(board).head.movements(board)
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
      val game = ChessGame.fromString(
        """....♖...
          |........
          |...↓....
          |...♙♟...
          |........
          |........
          |....♚...
          |........""".stripMargin)
      implicit val rules = game.rules.copy(whitePawnDirection = -1)

      val board = game.board
      game.whitePlayer.pieces(board).head.movements(board).size shouldBe 1
    }
```

- Support for Algebraic Notation (WIP)
```
TakeMovement(♝(XY(1, 3), WhiteChessPlayer), XY(1, -1), ♞(XY(2, 2), BlackChessPlayer)).toAn shouldBe "Bxc6"
```


## Use cases

- Making board game AIs
- UI API for board games (e.g. asking it "check mate?", or "which cells can I move to?" to highlight them)
- Solving/researching board games
- Inventing board games

## Short term TODO

- Start UI implementation with D3 and ScalaJS
- Start AI basic random implementation
- Make trivial "screensaver" Chess implementation to showcase the library
- Research repositories

## Long term TODO

- Implement PegSolitaireGame as proof of concept
- Solve PegSolitaireGame as proof of concept
- Implement Chess Game as proof of concept
- Implement partial/complete algebraic chess notation to Game: can display and practice from there
- Invent awesome game using this library
