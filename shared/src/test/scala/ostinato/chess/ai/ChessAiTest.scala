package ostinato.chess.ai

import org.scalatest._
import ostinato.chess.core._

class ChessAiTest extends FunSpec with Matchers {
  describe("ChessRandomAi") {
    it("should return the only available action") {
      new Fixture {
        val action = LoseAction(WhiteChessPlayer)

        override def _actions(implicit opts: ChessOptimisations) = Set(action)

        ChessRandomAi(WhiteChessPlayer).nextAction(game) shouldBe Some(action)
      }
    }
    it("should return random actions") {
      new Fixture {
        val seed = 1234L
        val random = new util.Random(seed)

        val action1 = LoseAction(WhiteChessPlayer)
        val action2 = LoseAction(BlackChessPlayer)
        val action3 = DrawAction(WhiteChessPlayer)

        val actions: Set[ChessAction] = Set(action1, action2, action3)
        val randomAction = random.shuffle(actions.toList).head

        override def _actions(implicit opts: ChessOptimisations) = actions

        val ai = ChessRandomAi(WhiteChessPlayer, Some(seed))
        ai.nextAction(game) shouldBe Some(randomAction)
      }
    }
  }

  trait Fixture {
    val opts = ChessOptimisations.default

    def _actions(implicit opts: ChessOptimisations): Set[ChessAction]

    lazy val board = new ChessBoard(grid = Vector()) {
      override def actions(implicit opts: ChessOptimisations) = _actions
    }

    lazy val game = new ChessGame(board, opts)
  }

}
