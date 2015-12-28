package ostinato.chess.ai

import org.scalatest._
import org.scalatest.mock.MockitoSugar
import ostinato.chess.core._
import org.mockito.Mockito.when
import org.mockito.Matchers.any

class ChessAiTest extends FunSpec with ShouldMatchers with MockitoSugar {
  describe("ChessRandomAi") {
    it("should return the only available action") {
      new Fixture {
        val action = mock[ChessAction]
        when(board.actions(any[ChessRules])).thenReturn(Set(action))
        ChessRandomAi(WhiteChessPlayer).nextAction(game) shouldBe Some(action)
      }
    }
    it("should return random actions") {
      new Fixture {
        val seed = 1234L
        val random = new util.Random(seed)

        val action1 = mock[ChessAction]
        val action2 = mock[ChessAction]
        val action3 = mock[ChessAction]

        val actions = Set(action1, action2, action3)
        val randomAction = random.shuffle(actions.toList).head

        when(board.actions(any[ChessRules])).thenReturn(actions)

        val ai = ChessRandomAi(WhiteChessPlayer, Some(seed))
        ai.nextAction(game) shouldBe Some(randomAction)
      }
    }
  }

  trait Fixture {
    val board = mock[ChessBoard]
    val game = new ChessGame(board, mock[ChessRules])
  }
}
