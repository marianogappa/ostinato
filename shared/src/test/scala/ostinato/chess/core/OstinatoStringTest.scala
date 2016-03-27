package ostinato.chess.core

import org.scalatest._
import ostinato.chess.InvalidIccfHistoryException
import ostinato.core.XY

import scala.util.{Failure, Success}

class OstinatoStringTest extends FunSpec with Matchers {
  describe("OstinatoString") {
    it("should be able to convert the basic ostinato string to a game") {
      ChessGame.fromOstinatoString(
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1") shouldBe Success(ChessGame.defaultGame)
    }

    it("should be able to convert a basic game to an ostinato string") {
      ChessGame.defaultGame.toOstinatoString shouldBe "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    }

    it("should be able to convert an ostinato string to a game, when it contains one history element") {
      val parsedGame = ChessGame.fromOstinatoString(
        "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1 5254")

      val baseExpectedGame = ChessGame.fromGridString(
        """♜♞♝♛♚♝♞♜
          |♟♟♟♟♟♟♟♟
          |........
          |........
          |....♙...
          |....↑...
          |♙♙♙♙.♙♙♙
          |♖♘♗♕♔♗♘♖
          |""".stripMargin, turn = BlackChessPlayer).get

      val expectedAction = EnPassantAction(♟(XY(4, 6), WhiteChessPlayer, -1), XY(0, -2))

      parsedGame.get.board.history shouldBe
        List(
          GameStep(Some(expectedAction), baseExpectedGame.board),
          GameStep(None, ChessGame.defaultGame.board)
        )
    }

    it("should be able to convert a game to an ostinato string, when it contains one history element") {
      val action = EnPassantAction(♟(XY(4, 6), WhiteChessPlayer, -1), XY(0, -2))
      ChessGame.defaultGame.board.doAction(action).get.toOstinatoString shouldBe
        "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1 5254"
    }

    it("should be able to convert an ostinato string to a game, when it contains two history elements") {
      val parsedGame = ChessGame.fromOstinatoString(
        "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1 5254 5756")

      val baseExpectedGame1 = ChessGame.fromGridString(
        """♜♞♝♛♚♝♞♜
          |♟♟♟♟♟♟♟♟
          |........
          |........
          |....♙...
          |....↑...
          |♙♙♙♙.♙♙♙
          |♖♘♗♕♔♗♘♖
          |""".stripMargin, turn = BlackChessPlayer).get

      val baseExpectedGame2 = ChessGame.fromGridString(
        """♜♞♝♛♚♝♞♜
          |♟♟♟♟.♟♟♟
          |....♟...
          |........
          |....♙...
          |........
          |♙♙♙♙.♙♙♙
          |♖♘♗♕♔♗♘♖
          |""".stripMargin, fullMoveNumber = 2).get

      val expectedAction1 = EnPassantAction(♟(XY(4, 6), WhiteChessPlayer, -1), XY(0, -2))
      val expectedAction2 = MoveAction(♟(XY(4, 1), BlackChessPlayer, 1), XY(0, 1))

      parsedGame.get.board.history shouldBe List(
        GameStep(Some(expectedAction2), baseExpectedGame2.board),
        GameStep(Some(expectedAction1), baseExpectedGame1.board),
        GameStep(None, ChessGame.defaultGame.board)
      )
    }

    it("should be able to convert a game to an ostinato string, when it contains two history elements") {
      val action1 = EnPassantAction(♟(XY(4, 6), WhiteChessPlayer, -1), XY(0, -2))
      val action2 = MoveAction(♟(XY(4, 1), BlackChessPlayer, 1), XY(0, 1))
      ChessGame.defaultGame.board.doAction(action1).get.doAction(action2).get.toOstinatoString shouldBe
        "rnbqkbnr/pppp1ppp/4p3/8/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2 5254 5756"
    }

    it("should not be able to parse invalid ostinato strings") {
      ChessGame.fromOstinatoString("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1 525blah!") match {
        case Failure(InvalidIccfHistoryException(_)) =>
        case _ => fail // N.B. "shouldBe 'failure" doesn't work on ScalaJS
      }

      ChessGame.fromOstinatoString("rnbqkbnr/pppppp$$pp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1 5254") shouldBe
        Failure(InvalidChessGridSizeException)

      ChessGame.fromOstinatoString("rnbqkbnr/pppppp$$pp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq$$ e3 0 1 5254") shouldBe
        Failure(OstinatoStringRegexMismatchException)
    }
  }
}
