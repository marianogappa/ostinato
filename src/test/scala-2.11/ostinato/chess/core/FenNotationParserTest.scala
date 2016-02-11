package ostinato.chess.core

import org.scalatest.{FunSpec, ShouldMatchers}

import scala.util.{Failure, Success}

class FenNotationParserTest extends FunSpec with ShouldMatchers {

  describe("to FEN Notation") {
    it("should go from and to FEN Notation") {
      val fenGame = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      ChessGame.fromFen(fenGame).get.toFen shouldBe fenGame
    }
    it("should encode a default ChessBoard to FEN Notation") {
      ChessGame.defaultGame.board.toFen shouldBe "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      ChessGame.defaultGame.board.toShortFen shouldBe "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
    }
    it("should encode this board") {
      ChessGame.fromGridString(
        """.......♔
          |........
          |♚.♙.....
          |.......♟
          |........
          |........
          |........
          |........
          |""".stripMargin).get.board.toShortFen shouldBe "7K/8/k1P5/7p/8/8/8/8"
    }
  }

  describe("from short FEN Notation") {
    it("should decode a default chess setup in short FEN Notation") {
      ChessGame.fromShortFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR") shouldBe Success(ChessGame.defaultGame)
    }
    it("should decode this chessboard in short FEN Notation") {
      ChessGame.fromShortFen("7K/8/k1P5/7p/8/8/8/8") shouldBe Success(ChessGame.fromGridString(
        """.......♔
          |........
          |♚.♙.....
          |.......♟
          |........
          |........
          |........
          |........
          |""".stripMargin).get)
    }
    it("should not decode an incomplete chessboard") {
      ChessGame.fromShortFen("7K/8/k1P5/7p/8") shouldBe Failure(InvalidChessGridSizeException)
    }
  }

  describe("from FEN Notation") {
    it("should decode a default chess setup in FEN Notation") {
      ChessGame.fromFen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") shouldBe Success(ChessGame.defaultGame)
    }
    it("should decode the default chess setup plus an e4 in FEN Notation") {
      ChessGame.fromFen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1") shouldBe
      Success(ChessGame.fromGridString(
      """♜♞♝♛♚♝♞♜
        |♟♟♟♟♟♟♟♟
        |........
        |........
        |....♙...
        |....↑...
        |♙♙♙♙.♙♙♙
        |♖♘♗♕♔♗♘♖
        |""".stripMargin, turn = BlackChessPlayer, castlingFullyAvailable, 1, 0).get)
    }
    it("should decode a chess setup with black en passant in FEN Notation") {
      ChessGame.fromFen("rnbqkbnr/p1pppppp/8/1p6/8/8/PPPPPPPP/RNBQKBNR w KQkq b6 4 5") shouldBe
      Success(ChessGame.fromGridString(
      """♜♞♝♛♚♝♞♜
        |♟.♟♟♟♟♟♟
        |.↓......
        |.♟......
        |........
        |........
        |♙♙♙♙♙♙♙♙
        |♖♘♗♕♔♗♘♖
        |""".stripMargin, turn = WhiteChessPlayer, castlingFullyAvailable, 5, 4).get)
    }

    val Q: Map[(ChessPlayer, CastlingSide.Value), Boolean] =
      castlingFullyUnavailable ++ Map((WhiteChessPlayer, CastlingSide.Queenside) -> true)
    val K: Map[(ChessPlayer, CastlingSide.Value), Boolean] =
      castlingFullyUnavailable ++ Map((WhiteChessPlayer, CastlingSide.Kingside) -> true)
    val q: Map[(ChessPlayer, CastlingSide.Value), Boolean] =
      castlingFullyUnavailable ++ Map((BlackChessPlayer, CastlingSide.Queenside) -> true)
    val k: Map[(ChessPlayer, CastlingSide.Value), Boolean] =
      castlingFullyUnavailable ++ Map((BlackChessPlayer, CastlingSide.Kingside) -> true)

    val castlingMap: List[(String, Map[(ChessPlayer, CastlingSide.Value), Boolean])] = List(
      ("Q", Q), ("K", K), ("q", q), ("k", k), ("KQkq", castlingFullyAvailable), ("-", castlingFullyUnavailable)
    )

    castlingMap foreach { case (k, v) => it(s"should decode a chess setup with castling '$k'") {
      ChessGame.fromFen(s"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w $k - 0 1") shouldBe
        Success(ChessGame.defaultGame.copy(board = ChessGame.defaultGame.board.copy(castlingAvailable = v)))
    }}
  }
}
