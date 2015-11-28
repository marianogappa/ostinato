import boardgame.core.XY
import org.scalatest.{ShouldMatchers, FunSpec}

import boardgame.chess.core._

class ChessGameTest extends FunSpec with ShouldMatchers {
  describe("ChessGame movements") {
    it("should print out a default starting game of Chess") {
      ChessGame.defaultGame.board.toString shouldBe
        """♜♞♝♛♚♝♞♜
          |♟♟♟♟♟♟♟♟
          |........
          |........
          |........
          |........
          |♙♙♙♙♙♙♙♙
          |♖♘♗♕♔♗♘♖""".stripMargin
    }

    it("should find 14 possible movements for a Rook") {
      val game = ChessGame.fromString(
        """........
          |........
          |........
          |...♜....
          |........
          |........
          |........
          |........""".stripMargin)

      movementCount(game, XY(3, 3)) shouldBe 14
    }

    it("should find 0 possible movements for a Rook") {
      val game = ChessGame.fromString(
        """........
          |........
          |...♜....
          |..♜♜♜...
          |...♜....
          |........
          |........
          |........""".stripMargin)

      movementCount(game, XY(3, 3)) shouldBe 0
    }

    it("should find 1 possible movements for a Rook") {
      val game = ChessGame.fromString(
        """........
          |........
          |...♜....
          |..♜♜♜...
          |...♖....
          |........
          |........
          |........""".stripMargin)

      movementCount(game, XY(3, 3)) shouldBe 1
    }

    it("should find 13 possible movements for a Bishop") {
      val game = ChessGame.fromString(
        """........
          |........
          |........
          |...♝....
          |........
          |........
          |........
          |........""".stripMargin)

      movementCount(game, XY(3, 3)) shouldBe 13
    }

    it("should find 0 possible movements for a Bishop") {
      val game = ChessGame.fromString(
        """........
          |........
          |..♞♞♞...
          |..♞♝♞...
          |..♞♞♞...
          |........
          |........
          |........""".stripMargin)

      movementCount(game, XY(3, 3)) shouldBe 0
    }

    it("should find 0 possible movements for a Bishop (diagonally trapped)") {
      val game = ChessGame.fromString(
        """........
          |........
          |..♞♞♞...
          |..♞♝♞...
          |..♞.♞...
          |........
          |........
          |........""".stripMargin)

      movementCount(game, XY(3, 3)) shouldBe 0
    }

    it("should find 0 possible movements for a Bishop (one diagonal free)") {
      val game = ChessGame.fromString(
        """........
          |........
          |..♞♞♞...
          |..♞♝♞...
          |....♞...
          |........
          |........
          |........""".stripMargin)

      movementCount(game, XY(3, 3)) shouldBe 3
    }

    it("should find 1 possible movements for a Bishop (take)") {
      val game = ChessGame.fromString(
        """........
          |........
          |..♞♞♞...
          |..♞♝♞...
          |..♗.♞...
          |........
          |........
          |........""".stripMargin)

      movementCount(game, XY(3, 3)) shouldBe 1
    }

    it("should find 8 possible movements for a Knight") {
      val game = ChessGame.fromString(
        """........
          |........
          |........
          |...♞....
          |........
          |........
          |........
          |........""".stripMargin)

      movementCount(game, XY(3, 3)) shouldBe 8
    }

    it("should find 0 possible movements for a Knight") {
      val game = ChessGame.fromString(
        """♞.......
          |..♞.....
          |.♞......
          |........
          |........
          |........
          |........
          |........""".stripMargin)

      movementCount(game, XY(0, 0)) shouldBe 0
    }

    it("should find 1 possible movements for a Knight") {
      val game = ChessGame.fromString(
        """♞.......
          |..♞.....
          |.♗......
          |........
          |........
          |........
          |........
          |........""".stripMargin)

      movementCount(game, XY(0, 0)) shouldBe 1
    }

    it("should find 27 possible movements for a Queen") {
      val game = ChessGame.fromString(
        """........
          |........
          |........
          |...♛....
          |........
          |........
          |........
          |........""".stripMargin)

      movementCount(game, XY(3, 3)) shouldBe 27
    }

    it("should find 7 possible movements for a Queen") {
      val game = ChessGame.fromString(
        """......♛♛
          |......♛.
          |........
          |........
          |........
          |........
          |........
          |........""".stripMargin)

      movementCount(game, XY(7, 0)) shouldBe 7
    }

    it("should find 1 possible movements for a Queen") {
      val game = ChessGame.fromString(
        """......♛♛
          |......♛♗
          |........
          |........
          |........
          |........
          |........
          |........""".stripMargin)

      movementCount(game, XY(7, 0)) shouldBe 1
    }

    it("should find 0 possible movements for a Queen; can't take King") {
      val game = ChessGame.fromString(
        """......♛♛
          |......♛♔
          |........
          |........
          |........
          |........
          |........
          |........""".stripMargin)

      movementCount(game, XY(7, 0)) shouldBe 0
    }

    it("should find 8 possible movements for a King") {
      val game = ChessGame.fromString(
        """........
          |........
          |........
          |...♔....
          |........
          |........
          |........
          |........""".stripMargin)

      movementCount(game, XY(3, 3)) shouldBe 8
    }

  }

  describe("ChessGame threatened/defended pieces") {
    it("should find no moves for King if it's threatened in every direction") {
      val game = ChessGame.fromString(
        """..♛.....
          |........
          |♛.......
          |...♔....
          |.......♛
          |........
          |........
          |....♛...""".stripMargin)

      val board = game.board

      implicit val rules = game.rules

      movementCount(game, XY(3, 3)) shouldBe 0
    }
    it("should find that the Queen is defended") {
      val game = ChessGame.fromString(
        """..♛.....
          |........
          |..♛.....
          |........
          |........
          |........
          |........
          |........""".stripMargin)
      val board = game.board
      implicit val rules = game.rules

      board.get(XY(2,2)).get.get.isDefended(board) shouldBe true
    }

    it("should find that the Queen is threatened") {
      val game = ChessGame.fromString(
        """..♖.....
          |........
          |..♛.....
          |........
          |........
          |........
          |........
          |........""".stripMargin)
      val board = game.board
      implicit val rules = game.rules

      board.get(XY(2,2)).get.get.isThreatened(board) shouldBe true
    }

    it("should find that the Queen is not threatened") {
      val game = ChessGame.fromString(
        """...♖....
          |........
          |..♛.....
          |........
          |........
          |........
          |........
          |........""".stripMargin)
      val board = game.board
      implicit val rules = game.rules
      val pieces = board.get(XY(2,2)).get.get.owner.pieces(board).toList

      board.get(XY(2,2)).get.get.isThreatened(board) shouldBe false
    }
  }

  describe("Game ending") {
    it("should find game over but not draw") {
      val game = ChessGame.fromString(
        """........
          |........
          |........
          |........
          |........
          |........
          |.....♛♛.
          |.......♔""".stripMargin)
      implicit val rules = game.rules
      game.isGameOver shouldBe true
      game.isDraw shouldBe false
    }

    it("should find draw and game over") {
      val game = ChessGame.fromString(
        """........
          |........
          |........
          |........
          |........
          |......♛.
          |........
          |.......♔""".stripMargin)
      implicit val rules = game.rules
      game.isDraw shouldBe true
      game.isGameOver shouldBe true
    }

    it("should not find draw nor game over") {
      val game = ChessGame.fromString(
        """........
          |........
          |........
          |........
          |......♛.
          |........
          |........
          |.......♔""".stripMargin)
      implicit val rules = game.rules
      game.isDraw shouldBe false
      game.isGameOver shouldBe false
    }

    it("should not find game over even if threatened") {
      val game = ChessGame.fromString(
        """........
          |........
          |........
          |........
          |........
          |........
          |......♛.
          |.......♔""".stripMargin)
      implicit val rules = game.rules
      game.isGameOver shouldBe false
    }

    it("white should have two CheckMateMovements available") {
      val game = ChessGame.fromString(
        """........
          |........
          |........
          |........
          |.....♛..
          |......♛.
          |........
          |.......♔""".stripMargin)
      implicit val rules = game.rules
      val board = game.board

      board.get(XY(5, 4)).get.get.owner.movements(board).filter {
        case m: ChessMovement => m.isCheckmate
        case _ => false
      }.size shouldBe 7
    }
  }

  describe("Pawns") {
    it("should find 1 possible move for white pawn") {
      val game = ChessGame.fromString(
        """........
          |........
          |..♟.....
          |........
          |........
          |........
          |........
          |........""".stripMargin)
      implicit val rules = game.rules

      movementCount(game, XY(2, 2)) shouldBe 1
    }

    it("should find 2 possible moves for white pawn") {
      val game = ChessGame.fromString(
        """........
          |..♟.....
          |........
          |........
          |........
          |........
          |........
          |........""".stripMargin)
      implicit val rules = game.rules

      movementCount(game, XY(2, 1)) shouldBe 2
    }

    it("should find 4 possible moves for white pawn") {
      val game = ChessGame.fromString(
        """........
          |..♟.....
          |.♖.♖....
          |........
          |........
          |........
          |........
          |........""".stripMargin)
      implicit val rules = game.rules

      movementCount(game, XY(2, 1)) shouldBe 4
    }

    it("should find 3 possible moves for white pawn") {
      val game = ChessGame.fromString(
        """........
          |........
          |..♟.....
          |.♖.♖....
          |........
          |........
          |........
          |........""".stripMargin)
      implicit val rules = game.rules

      movementCount(game, XY(2, 2)) shouldBe 3
    }

    it("should find 0 possible moves for white pawn") {
      val game = ChessGame.fromString(
        """........
          |..♟.....
          |..♖.....
          |........
          |........
          |........
          |........
          |........""".stripMargin)
      implicit val rules = game.rules

      movementCount(game, XY(2, 1), true) shouldBe 0
    }

    it("should not find white pawn in promoting position") {
      val game = ChessGame.fromString(
        """..♟.....
          |........
          |........
          |........
          |........
          |........
          |........
          |........""".stripMargin)
      implicit val rules = game.rules

      game.board.get(XY(2, 0)).get.get match {
        case p: ♟ => p.isPromoting shouldBe false
        case _ => fail
      }
    }

    it("should find white pawn in promoting position") {
      val game = ChessGame.fromString(
        """........
          |........
          |........
          |........
          |........
          |........
          |........
          |..♟.....""".stripMargin)
      implicit val rules = game.rules

      game.board.get(XY(2, 7)).get.get match {
        case p: ♟ => p.isPromoting shouldBe true
        case _ => fail
      }
    }

    it("should not find black pawn in promoting position") {
      val game = ChessGame.fromString(
        """........
          |........
          |........
          |........
          |........
          |........
          |........
          |..♙.....""".stripMargin)
      implicit val rules = game.rules

      game.board.get(XY(2, 7)).get.get match {
        case p: ♟ => p.isPromoting shouldBe false
        case _ => fail
      }
    }

    it("should find black pawn in promoting position") {
      val game = ChessGame.fromString(
        """..♙.....
          |........
          |........
          |........
          |........
          |........
          |........
          |........""".stripMargin)
      implicit val rules = game.rules

      game.board.get(XY(2, 0)).get.get match {
        case p: ♟ => p.isPromoting shouldBe true
        case _ => fail
      }
    }

    it("should find the 4 promoting moves for pawn") {
      val game = ChessGame.fromString(
        """........
          |..♙.....
          |........
          |........
          |........
          |........
          |........
          |........""".stripMargin)
      implicit val rules = game.rules

      movementCount(game, XY(2, 1)) shouldBe 4
    }
  }

  describe("En Passant") {
    it("should find only en passant take movement for white pawn") {
      val game = ChessGame.fromString(
        """........
          |........
          |........
          |........
          |........
          |..♟♙....
          |..♟↑....
          |........""".stripMargin)
      implicit val rules = game.rules

      val board = game.board

      val movements = board.get(XY(2,5)).get.get.movements(board)

      movements.size shouldBe 1
      movements.toList.head shouldBe a [EnPassantTakeMovement]
    }
    it("should find 2 moves including en passant for black pawn") {
      val game = ChessGame.fromString(
        """........
          |........
          |...↓....
          |...♟♙...
          |........
          |........
          |........
          |........""".stripMargin)
      implicit val rules = game.rules

      movementCount(game, XY(4, 3)) shouldBe 2
    }
    it("should not find en passant take move for black pawn, since king would be threatened") {
      val game = ChessGame.fromString(
        """....♜...
          |........
          |...↓....
          |...♙♟...
          |........
          |........
          |....♔...
          |........""".stripMargin)
      implicit val rules = game.rules

      movementCount(game, XY(4, 3)) shouldBe 1
    }
  }

  describe("King's initial position") {
    it("should determine that black king is in initial position, if white is on top") {
        val game = ChessGame.fromString(
          """........
            |........
            |........
            |........
            |........
            |........
            |........
            |....♔...""".stripMargin)
        implicit val rules = game.rules

        game.blackPlayer.kingPiece(game.board) match {
          case Some(k: ♚) => k.isInInitialPosition shouldBe true
          case _ => fail
        }
    }
    it("should determine that black king is NOT in initial position, if white is on top") {
        val game = ChessGame.fromString(
          """........
            |........
            |........
            |........
            |........
            |........
            |....♔...
            |........""".stripMargin)
        implicit val rules = game.rules
        val board = game.board

        game.blackPlayer.kingPiece(game.board) match {
          case Some(k: ♚) => k.isInInitialPosition shouldBe false
          case _ => fail
        }
    }
    it("should determine that black king is NOT in initial position if it's in white's initial position, if white is on top") {
        val game = ChessGame.fromString(
          """....♔...
            |........
            |........
            |........
            |........
            |........
            |........
            |........""".stripMargin)
        implicit val rules = game.rules
        val board = game.board

      game.blackPlayer.kingPiece(game.board) match {
          case Some(k: ♚) => k.isInInitialPosition shouldBe false
          case _ => fail
        }
    }
    it("should determine that white king is in initial position, if black is on top") {
        val game = ChessGame.fromString(
          """....♚...
            |........
            |........
            |........
            |........
            |........
            |........
            |........""".stripMargin)
        implicit val rules = game.rules

        game.whitePlayer.kingPiece(game.board) match {
          case Some(k: ♚) => k.isInInitialPosition shouldBe true
          case _ => fail
        }
    }
    it("should determine that white king is NOT in initial position, if black is on top") {
        val game = ChessGame.fromString(
          """........
            |........
            |....♚...
            |........
            |........
            |........
            |........
            |........""".stripMargin)
        implicit val rules = game.rules
        val board = game.board

        game.whitePlayer.kingPiece(game.board) match {
          case Some(k: ♚) => k.isInInitialPosition shouldBe false
          case _ => fail
        }
    }
    it("should determine that white king is NOT in initial position if it's in white's initial position, if black is on top") {
        val game = ChessGame.fromString(
          """........
            |........
            |........
            |........
            |........
            |........
            |........
            |....♚...""".stripMargin)
        implicit val rules = game.rules
        val board = game.board

      game.whitePlayer.kingPiece(game.board) match {
          case Some(k: ♚) => k.isInInitialPosition shouldBe false
          case _ => fail
        }
    }

    describe("Castling") {
      it("should determine that white king can castle") {
        val game = ChessGame.fromString(
          """....♚..♜
            |........
            |........
            |........
            |........
            |........
            |........
            |........""".stripMargin)
        implicit val rules = game.rules
        val board = game.board

        game.whitePlayer.movements(board).exists {
          case m: CastlingMovement => true
          case _ => false
        } shouldBe true
      }
      it("should determine that white king can't castle because it's not in initial position") {
        val game = ChessGame.fromString(
          """...♚...♜
            |........
            |........
            |........
            |........
            |........
            |........
            |........""".stripMargin)
        implicit val rules = game.rules
        val board = game.board

        game.whitePlayer.movements(board).forall {
          case m: CastlingMovement => false
          case _ => true
        } shouldBe true
      }
      it("should determine that white king can't castle because target rook is not in initial position") {
        val game = ChessGame.fromString(
          """....♚.♜.
            |........
            |........
            |........
            |........
            |........
            |........
            |........""".stripMargin)
        implicit val rules = game.rules
        val board = game.board

        game.whitePlayer.movements(board).forall {
          case m: CastlingMovement => false
          case _ => true
        } shouldBe true
      }
      it("should determine that white king can't castle because white is on top unless otherwise specified") {
        val game = ChessGame.fromString(
          """........
            |........
            |........
            |........
            |........
            |........
            |........
            |....♚..♜""".stripMargin)
        implicit val rules = game.rules
        val board = game.board

        game.whitePlayer.movements(board).forall {
          case m: CastlingMovement => false
          case _ => true
        } shouldBe true
      }
      it("should determine that white king can castle because white is specified to be on the bottom") {
        val game = ChessGame.fromString(
          """........
            |........
            |........
            |........
            |........
            |........
            |........
            |....♚..♜""".stripMargin)
        implicit val rules = game.rules.copy(whitePawnDirection = -1)
        val board = game.board

        game.whitePlayer.movements(board).exists {
          case m: CastlingMovement => true
          case _ => false
        } shouldBe true
      }
      it("should determine that white king can't castle because the king is threatened") {
        val game = ChessGame.fromString(
          """........
            |........
            |........
            |........
            |........
            |........
            |....♖...
            |....♚..♜""".stripMargin)
        implicit val rules = game.rules.copy(whitePawnDirection = -1)
        val board = game.board

        game.whitePlayer.movements(board).exists {
          case m: CastlingMovement => true
          case _ => false
        } shouldBe false
      }
      it("should determine that white king can't castle because a piece the king will pass through is threatened") {
        val game = ChessGame.fromString(
          """........
            |........
            |........
            |........
            |........
            |........
            |.....♖..
            |....♚..♜""".stripMargin)
        implicit val rules = game.rules.copy(whitePawnDirection = -1)
        val board = game.board

        game.whitePlayer.movements(board).exists {
          case m: CastlingMovement => true
          case _ => false
        } shouldBe false
      }
      it("should determine that white king can't long castle because a piece the king will pass through is threatened") {
        val game = ChessGame.fromString(
          """........
            |........
            |........
            |........
            |........
            |........
            |..♖.....
            |♜...♚...""".stripMargin)
        implicit val rules = game.rules.copy(whitePawnDirection = -1)
        val board = game.board

        game.whitePlayer.movements(board).exists {
          case m: CastlingMovement => true
          case _ => false
        } shouldBe false
      }
      it("should determine that white king can long castle") {
        val game = ChessGame.fromString(
          """........
            |........
            |........
            |........
            |........
            |........
            |.....♖..
            |♜...♚...""".stripMargin)
        implicit val rules = game.rules.copy(whitePawnDirection = -1)
        val board = game.board

        game.whitePlayer.movements(board).exists {
          case m: CastlingMovement => true
          case _ => false
        } shouldBe true
      }
    }

    describe("Algebraic notation") {
      it("should find rook at h8 if white pawn moves downwards") {
        val game = ChessGame.fromString(
          """........
            |........
            |........
            |........
            |........
            |........
            |........
            |♜.......""".stripMargin)
        val board = game.board
        implicit val rules = game.rules

        An.fromXY(game.whitePlayer.pieces(board).head.pos) shouldBe An('h', 8)
      }
      it("should find rook at a1 if white pawn moves upwards") {
        val game = ChessGame.fromString(
          """........
            |........
            |........
            |........
            |........
            |........
            |........
            |♜.......""".stripMargin)
        val board = game.board
        implicit val rules = game.rules.copy(whitePawnDirection = -1)

        An.fromXY(game.whitePlayer.pieces(board).head.pos) shouldBe An('a', 1)
      }
      it("should find black rook at e4 if white pawn moves downwards") {
        val game = ChessGame.fromString(
          """........
            |........
            |........
            |...♖....
            |........
            |........
            |........
            |........""".stripMargin)
        val board = game.board
        implicit val rules = game.rules

        An.fromXY(game.blackPlayer.pieces(board).head.pos) shouldBe An('e', 4)
      }
    }
  }

  private def movementCount(game: ChessGame, point: XY, show: Boolean = false) = {
    val board = game.board
    implicit val rules = game.rules

    val movements = board.get(point).get.get.movements(board)
    if (show) movements map board.move foreach (b => println(b + "\n"))

    movements.size
  }
}
