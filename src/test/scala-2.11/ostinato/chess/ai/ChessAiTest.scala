package ostinato.chess.ai

import org.scalatest._
import org.scalatest.mock.MockitoSugar
import ostinato.chess.core._
import org.mockito.Mockito.when
import org.mockito.Matchers.any

class ChessAiTest extends FunSpec with ShouldMatchers with MockitoSugar {
  describe("ChessRandomAi") {
    it("should Draw if there are no movements available") {
      new Fixture {
        when(board.movements(any[ChessRules])).thenReturn(Set.empty[ChessMovement])
        ChessRandomAi(WhiteChessPlayer).move(game) shouldBe a[DrawMovement]
      }
    }
    it("should return the only available movement") {
      new Fixture {
        val movement = mock[ChessMovement]
        when(board.movements(any[ChessRules])).thenReturn(Set(movement))
        ChessRandomAi(WhiteChessPlayer).move(game) shouldBe movement
      }
    }
    it("should return random movements") {
      new Fixture {
        val seed = 1234L
        val random = new util.Random(seed)

        val movement1 = mock[ChessMovement]
        val movement2 = mock[ChessMovement]
        val movement3 = mock[ChessMovement]

        val movements = Set(movement1, movement2, movement3)
        val randomMovement = random.shuffle(movements.toList).head

        when(board.movements(any[ChessRules])).thenReturn(movements)

        val ai = ChessRandomAi(WhiteChessPlayer, Some(seed))
        ai.move(game) shouldBe randomMovement
      }
    }
  }

  trait Fixture {
    val board = mock[ChessBoard]
    val game = new ChessGame(board, mock[ChessRules])
  }
}
