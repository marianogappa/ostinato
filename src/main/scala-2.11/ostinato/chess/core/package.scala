package ostinato.chess

import ostinato.core.{ XY, BoardSize }

package object core {
  implicit val chessBoardSize = BoardSize(8, 8)

  object CastlingSide extends Enumeration {
    type CastlingSide = Value
    val Queenside, Kingside = Value
  }

  object SquareColor extends Enumeration {
    type SquareColor = Value
    val Light, Dark = Value
  }

  lazy val chessPlayers: List[ChessPlayer] = List(WhiteChessPlayer, BlackChessPlayer)
  lazy val castlingSides = List(CastlingSide.Queenside, CastlingSide.Kingside)

  lazy val castlingFullyAvailable: Map[(ChessPlayer, CastlingSide.Value), Boolean] = (for {
    chessPlayer ← chessPlayers
    castlingSide ← castlingSides
  } yield (chessPlayer, castlingSide) -> true).toMap

  lazy val castlingFullyUnavailable = castlingFullyAvailable map (kv => (kv._1, false))
  lazy val castlingOnlyBlackAvailable = castlingFullyAvailable map { case ((p, s), v) => ((p, s), p == BlackChessPlayer) }
  lazy val castlingOnlyWhiteAvailable = castlingFullyAvailable map { case ((p, s), v) => ((p, s), p == WhiteChessPlayer) }

  def fenCastling(castlingAvailable: Map[(ChessPlayer, CastlingSide.Value), Boolean]) =
    if (castlingAvailable == castlingFullyUnavailable)
      "-"
    else
      List(
        "K" -> castlingAvailable((WhiteChessPlayer, CastlingSide.Kingside)),
        "Q" -> castlingAvailable((WhiteChessPlayer, CastlingSide.Queenside)),
        "k" -> castlingAvailable((BlackChessPlayer, CastlingSide.Kingside)),
        "q" -> castlingAvailable((BlackChessPlayer, CastlingSide.Queenside))
      ).filter(_._2).map(_._1).mkString

  object ChessXY {
    lazy val chars = "abcdefgh"
    def fromAn(string: String)(implicit rules: ChessRules = ChessRules.default) = {
        val s = string.filter(_ > ' ').toLowerCase
      if (s.length == 2 && s.matches("""[a-h][1-8]"""))
        if (rules.whitePawnDirection == 1)
          Some(XY(chars.indexOf(s(0)), s(1).asDigit - 1))
        else
          Some(XY(chars.indexOf(s(0)), chessBoardSize.y - 1 - (s(1).asDigit - 1)))
      else
        None
    }
  }

  object ChessGrid {
    def fromGridString(s: String)(implicit rules: ChessRules = ChessRules.default): Vector[Option[ChessPiece]] = {
      charVector(s) map {
        case ('♜', i) ⇒ Some(♜(XY.fromI(i), BlackChessPlayer))
        case ('♞', i) ⇒ Some(♞(XY.fromI(i), BlackChessPlayer))
        case ('♝', i) ⇒ Some(♝(XY.fromI(i), BlackChessPlayer))
        case ('♛', i) ⇒ Some(♛(XY.fromI(i), BlackChessPlayer))
        case ('♚', i) ⇒ Some(♚(XY.fromI(i), BlackChessPlayer))
        case ('♟', i) ⇒ Some(♟(XY.fromI(i), BlackChessPlayer, rules.whitePawnDirection * -1))
        case ('♖', i) ⇒ Some(♜(XY.fromI(i), WhiteChessPlayer))
        case ('♘', i) ⇒ Some(♞(XY.fromI(i), WhiteChessPlayer))
        case ('♗', i) ⇒ Some(♝(XY.fromI(i), WhiteChessPlayer))
        case ('♕', i) ⇒ Some(♛(XY.fromI(i), WhiteChessPlayer))
        case ('♔', i) ⇒ Some(♚(XY.fromI(i), WhiteChessPlayer))
        case ('♙', i) ⇒ Some(♟(XY.fromI(i), WhiteChessPlayer, rules.whitePawnDirection))
        case _        ⇒ None
      }
    }
    def charVector(s: String) = s.split('\n').mkString.zipWithIndex.toVector
  }

  implicit class ChessXY(pos: XY) {
    def squareColor = if ((pos.x + pos.y) % 2 == 0) SquareColor.Light else SquareColor.Dark

    def toAn(implicit rules: ChessRules = ChessRules.default, chessBoardSize: BoardSize) = {
      if (rules.whitePawnDirection == 1)
        AnPos(ChessXY.chars(chessBoardSize.x - 1 - pos.x), pos.y + 1)
      else
        AnPos(ChessXY.chars(pos.x), chessBoardSize.y - pos.y)
    }

    lazy val dnConversions =
      Map('a' -> Set("QR", "R"), 'b' -> Set("QN", "N"), 'c' -> Set("QB", "B"), 'd' -> Set("Q"), 'e' -> Set("K"),
        'f' -> Set("KB", "B"), 'g' -> Set("KN", "N"), 'h' -> Set("KR", "R"))

    lazy val iccfConversions =
      Map('a' -> 1, 'b' -> 2, 'c' -> 3, 'd' -> 4, 'e' -> 5, 'f' -> 6, 'g' -> 7, 'h' -> 8)

    def toDn(turn: ChessPlayer)(implicit rules: ChessRules = ChessRules.default, chessBoardSize: BoardSize) = {
      (toAn, turn) match {
        case (AnPos(x, y), WhiteChessPlayer) => dnConversions(x) map (DnPos(_, y))
        case (AnPos(x, y), BlackChessPlayer) => dnConversions(x) map (DnPos(_, 9 - y))
      }
    }

    def toIccf(implicit rules: ChessRules = ChessRules.default, chessBoardSize: BoardSize) = {
      val an = toAn
      IccfPos(iccfConversions(an.x), an.y)
    }
  }

  case class AnPos(x: Char, y: Int) {
    override def toString = s"$x$y"
  }

  case class DnPos(x: String, y: Int) {
    override def toString = s"$x$y"
  }

  case class IccfPos(x: Int, y: Int) {
    override def toString = s"$x$y"
  }

  object Fan {
    def checkmate(winner: ChessPlayer) = if (winner == WhiteChessPlayer) "1-0" else "0-1"
    def check(implicit rules: ChessRules = ChessRules.default) = "+"
    def kingSideCastle(implicit rules: ChessRules = ChessRules.default) = "0-0"
    def queenSideCastle(implicit rules: ChessRules = ChessRules.default) = "0-0-0"
    def draw(implicit rules: ChessRules = ChessRules.default) = "½–½"
  }

  case class PastBoards(boards: Map[String, Int] = Map()) {
    def serialiseBoard(board: ChessBoard): String = "^([^ ]+ +[^ ]+ +[^ ]+ +[^ ]+).*$".r.replaceFirstIn(board.toShortFen, "$1")
    lazy val isInThreefoldRepetition = boards.values.exists(_ >= 3)
    val isEmpty = boards.isEmpty

    def withBoard(board: ChessBoard) = {
      val serialisedBoard = serialiseBoard(board)
      if (boards.get(serialisedBoard).nonEmpty)
        PastBoards(boards.updated(serialisedBoard, boards(serialisedBoard) + 1))
      else
        PastBoards(boards ++ Map(serialisedBoard -> 1))
    }
  }

  // N.B. this optimisation can appear ugly, but it's the only reason this library is fast
  def posThreatenedBy(pos: XY, player: ChessPlayer, board: ChessBoard)(implicit rules: ChessRules = ChessRules.default): Option[ChessPiece] = {

    def isEnemyKnight(pos: XY) = board.get(pos) match { case Some(Some(p)) if p.owner == player.enemy && p.isKnight ⇒ true; case _ ⇒ false }
    def isEnemyQueenOrKingOrRook(pos: XY) = board.get(pos) match { case Some(Some(p)) if p.owner == player.enemy && (p.isQueen || p.isKing || p.isRook) ⇒ true; case _ ⇒ false }
    def isEnemyQueenOrKingOrBishop(pos: XY) = board.get(pos) match { case Some(Some(p)) if p.owner == player.enemy && (p.isQueen || p.isKing || p.isBishop) ⇒ true; case _ ⇒ false }
    def isEnemyQueenOrKingOrBishopOrPawn(pos: XY) = board.get(pos) match { case Some(Some(p)) if p.owner == player.enemy && (p.isQueen || p.isKing || p.isBishop || p.isPawn) ⇒ true; case _ ⇒ false }
    def isEnemyQueenOrRook(pos: XY) = board.get(pos) match { case Some(Some(p)) if p.owner == player.enemy && (p.isQueen || p.isRook) ⇒ true; case _ ⇒ false }
    def isEnemyQueenOrBishop(pos: XY) = board.get(pos) match { case Some(Some(p)) if p.owner == player.enemy && (p.isQueen || p.isBishop) ⇒ true; case _ ⇒ false }
    def isPiece(pos: XY) = board.isPiece(board.get(pos))

    if (isEnemyKnight(pos + XY(-1, -2)))
      board.get(pos + XY(-1, -2)).flatten
    else if (isEnemyKnight(pos + XY(1, -2)))
      board.get(pos + XY(1, -2)).flatten
    else if (isEnemyKnight(pos + XY(-1, 2)))
      board.get(pos + XY(-1, 2)).flatten
    else if (isEnemyKnight(pos + XY(1, 2)))
      board.get(pos + XY(1, 2)).flatten
    else if (isEnemyKnight(pos + XY(-2, -1)))
      board.get(pos + XY(-2, -1)).flatten
    else if (isEnemyKnight(pos + XY(-2, 1)))
      board.get(pos + XY(-2, 1)).flatten
    else if (isEnemyKnight(pos + XY(2, -1)))
      board.get(pos + XY(2, -1)).flatten
    else if (isEnemyKnight(pos + XY(2, 1)))
      board.get(pos + XY(2, 1)).flatten
    else {

      if (isEnemyQueenOrKingOrRook(pos + XY(0, -1))) {
        return board.get(pos + XY(0, -1)).flatten
      } else if ((pos + XY(0, -1)).exists && !isPiece(pos + XY(0, -1))) {
        if (isEnemyQueenOrRook(pos + XY(0, -2))) {
          return board.get(pos + XY(0, -2)).flatten
        } else if ((pos + XY(0, -2)).exists && !isPiece(pos + XY(0, -2))) {
          if (isEnemyQueenOrRook(pos + XY(0, -3))) {
            return board.get(pos + XY(0, -3)).flatten
          } else if ((pos + XY(0, -3)).exists && !isPiece(pos + XY(0, -3))) {
            if (isEnemyQueenOrRook(pos + XY(0, -4))) {
              return board.get(pos + XY(0, -4)).flatten
            } else if ((pos + XY(0, -4)).exists && !isPiece(pos + XY(0, -4))) {
              if (isEnemyQueenOrRook(pos + XY(0, -5))) {
                return board.get(pos + XY(0, -5)).flatten
              } else if ((pos + XY(0, -5)).exists && !isPiece(pos + XY(0, -5))) {
                if (isEnemyQueenOrRook(pos + XY(0, -6))) {
                  return board.get(pos + XY(0, -6)).flatten
                } else if ((pos + XY(0, -6)).exists && !isPiece(pos + XY(0, -6))) {
                  if (isEnemyQueenOrRook(pos + XY(0, -7)))
                    return board.get(pos + XY(0, -7)).flatten
                }
              }
            }
          }
        }
      }

      if (isEnemyQueenOrKingOrRook(pos + XY(0, 1))) {
        return board.get(pos + XY(0, 1)).flatten
      } else if ((pos + XY(0, 1)).exists && !isPiece(pos + XY(0, 1))) {
        if (isEnemyQueenOrRook(pos + XY(0, 2))) {
          return board.get(pos + XY(0, 2)).flatten
        } else if ((pos + XY(0, 2)).exists && !isPiece(pos + XY(0, 2))) {
          if (isEnemyQueenOrRook(pos + XY(0, 3))) {
            return board.get(pos + XY(0, 3)).flatten
          } else if ((pos + XY(0, 3)).exists && !isPiece(pos + XY(0, 3))) {
            if (isEnemyQueenOrRook(pos + XY(0, 4))) {
              return board.get(pos + XY(0, 4)).flatten
            } else if ((pos + XY(0, 4)).exists && !isPiece(pos + XY(0, 4))) {
              if (isEnemyQueenOrRook(pos + XY(0, 5))) {
                return board.get(pos + XY(0, 5)).flatten
              } else if ((pos + XY(0, 5)).exists && !isPiece(pos + XY(0, 5))) {
                if (isEnemyQueenOrRook(pos + XY(0, 6))) {
                  return board.get(pos + XY(0, 6)).flatten
                } else if ((pos + XY(0, 6)).exists && !isPiece(pos + XY(0, 6))) {
                  if (isEnemyQueenOrRook(pos + XY(0, 7)))
                    return board.get(pos + XY(0, 7)).flatten
                }
              }
            }
          }
        }
      }

      if (isEnemyQueenOrKingOrBishopOrPawn(pos + XY(1, 1))) {
        val enemyPos = pos + XY(1, 1)
        val enemy = player.enemy
        board.get(enemyPos) match {
          case Some(Some(p)) if !p.isPawn ⇒
            return board.get(enemyPos).flatten
          case Some(Some(♟(`enemyPos`, `enemy`, -1))) ⇒
            return board.get(enemyPos).flatten
          case _ ⇒
        }
      } else if ((pos + XY(1, 1)).exists && !isPiece(pos + XY(1, 1))) {
        if (isEnemyQueenOrBishop(pos + XY(2, 2))) {
          return board.get(pos + XY(2, 2)).flatten
        } else if ((pos + XY(2, 2)).exists && !isPiece(pos + XY(2, 2))) {
          if (isEnemyQueenOrBishop(pos + XY(3, 3))) {
            return board.get(pos + XY(3, 3)).flatten
          } else if ((pos + XY(3, 3)).exists && !isPiece(pos + XY(3, 3))) {
            if (isEnemyQueenOrBishop(pos + XY(4, 4))) {
              return board.get(pos + XY(4, 4)).flatten
            } else if ((pos + XY(4, 4)).exists && !isPiece(pos + XY(4, 4))) {
              if (isEnemyQueenOrBishop(pos + XY(5, 5))) {
                return board.get(pos + XY(5, 5)).flatten
              } else if ((pos + XY(5, 5)).exists && !isPiece(pos + XY(5, 5))) {
                if (isEnemyQueenOrBishop(pos + XY(6, 6))) {
                  return board.get(pos + XY(6, 6)).flatten
                } else if ((pos + XY(6, 6)).exists && !isPiece(pos + XY(6, 6))) {
                  if (isEnemyQueenOrBishop(pos + XY(7, 7)))
                    return board.get(pos + XY(7, 7)).flatten
                }
              }
            }
          }
        }
      }

      if (isEnemyQueenOrKingOrBishopOrPawn(pos + XY(-1, 1))) {
        val enemyPos = pos + XY(-1, 1)
        val enemy = player.enemy
        board.get(enemyPos) match {
          case Some(Some(p)) if !p.isPawn ⇒
            return board.get(enemyPos).flatten
          case Some(Some(♟(`enemyPos`, `enemy`, -1))) ⇒
            return board.get(enemyPos).flatten
          case _ ⇒
        }
        return board.get(pos + XY(-1, 1)).flatten
      } else if ((pos + XY(-1, 1)).exists && !isPiece(pos + XY(-1, 1))) {
        if (isEnemyQueenOrBishop(pos + XY(-2, 2))) {
          return board.get(pos + XY(-2, 2)).flatten
        } else if ((pos + XY(-2, 2)).exists && !isPiece(pos + XY(-2, 2))) {
          if (isEnemyQueenOrBishop(pos + XY(-3, 3))) {
            return board.get(pos + XY(-3, 3)).flatten
          } else if ((pos + XY(-3, 3)).exists && !isPiece(pos + XY(-3, 3))) {
            if (isEnemyQueenOrBishop(pos + XY(-4, 4))) {
              return board.get(pos + XY(-4, 4)).flatten
            } else if ((pos + XY(-4, 4)).exists && !isPiece(pos + XY(-4, 4))) {
              if (isEnemyQueenOrBishop(pos + XY(-5, 5))) {
                return board.get(pos + XY(-5, 5)).flatten
              } else if ((pos + XY(-5, 5)).exists && !isPiece(pos + XY(-5, 5))) {
                if (isEnemyQueenOrBishop(pos + XY(-6, 6))) {
                  return board.get(pos + XY(-6, 6)).flatten
                } else if ((pos + XY(-6, 6)).exists && !isPiece(pos + XY(-6, 6))) {
                  if (isEnemyQueenOrBishop(pos + XY(-7, 7)))
                    return board.get(pos + XY(-7, 7)).flatten
                }
              }
            }
          }
        }
      }

      if (isEnemyQueenOrKingOrBishopOrPawn(pos + XY(-1, -1))) {
        val enemyPos = pos + XY(-1, -1)
        val enemy = player.enemy
        board.get(enemyPos) match {
          case Some(Some(p)) if !p.isPawn ⇒
            return board.get(enemyPos).flatten
          case Some(Some(♟(`enemyPos`, `enemy`, 1))) ⇒
            return board.get(enemyPos).flatten
          case _ ⇒
        }
        return board.get(pos + XY(-1, -1)).flatten
      } else if ((pos + XY(-1, -1)).exists && !isPiece(pos + XY(-1, -1))) {
        if (isEnemyQueenOrBishop(pos + XY(-2, -2))) {
          return board.get(pos + XY(-2, -2)).flatten
        } else if ((pos + XY(-2, -2)).exists && !isPiece(pos + XY(-2, -2))) {
          if (isEnemyQueenOrBishop(pos + XY(-3, -3))) {
            return board.get(pos + XY(-3, -3)).flatten
          } else if ((pos + XY(-3, -3)).exists && !isPiece(pos + XY(-3, -3))) {
            if (isEnemyQueenOrBishop(pos + XY(-4, -4))) {
              return board.get(pos + XY(-4, -4)).flatten
            } else if ((pos + XY(-4, -4)).exists && !isPiece(pos + XY(-4, -4))) {
              if (isEnemyQueenOrBishop(pos + XY(-5, -5))) {
                return board.get(pos + XY(-5, -5)).flatten
              } else if ((pos + XY(-5, -5)).exists && !isPiece(pos + XY(-5, -5))) {
                if (isEnemyQueenOrBishop(pos + XY(-6, -6))) {
                  return board.get(pos + XY(-6, -6)).flatten
                } else if ((pos + XY(-6, -6)).exists && !isPiece(pos + XY(-6, -6))) {
                  if (isEnemyQueenOrBishop(pos + XY(-7, -7)))
                    return board.get(pos + XY(-7, -7)).flatten
                }
              }
            }
          }
        }
      }

      if (isEnemyQueenOrKingOrBishopOrPawn(pos + XY(1, -1))) {
        val enemyPos = pos + XY(1, -1)
        val enemy = player.enemy
        board.get(enemyPos) match {
          case Some(Some(p)) if !p.isPawn ⇒
            return board.get(enemyPos).flatten
          case Some(Some(♟(`enemyPos`, `enemy`, 1))) ⇒
            return board.get(enemyPos).flatten
          case _ ⇒
        }
        return board.get(pos + XY(1, -1)).flatten
      } else if ((pos + XY(1, -1)).exists && !isPiece(pos + XY(1, -1))) {
        if (isEnemyQueenOrBishop(pos + XY(2, -2))) {
          return board.get(pos + XY(2, -2)).flatten
        } else if ((pos + XY(2, -2)).exists && !isPiece(pos + XY(2, -2))) {
          if (isEnemyQueenOrBishop(pos + XY(3, -3))) {
            return board.get(pos + XY(3, -3)).flatten
          } else if ((pos + XY(3, -3)).exists && !isPiece(pos + XY(3, -3))) {
            if (isEnemyQueenOrBishop(pos + XY(4, -4))) {
              return board.get(pos + XY(4, -4)).flatten
            } else if ((pos + XY(4, -4)).exists && !isPiece(pos + XY(4, -4))) {
              if (isEnemyQueenOrBishop(pos + XY(5, -5))) {
                return board.get(pos + XY(5, -5)).flatten
              } else if ((pos + XY(5, -5)).exists && !isPiece(pos + XY(5, -5))) {
                if (isEnemyQueenOrBishop(pos + XY(6, -6))) {
                  return board.get(pos + XY(6, -6)).flatten
                } else if ((pos + XY(6, -6)).exists && !isPiece(pos + XY(6, -6))) {
                  if (isEnemyQueenOrBishop(pos + XY(7, -7)))
                    return board.get(pos + XY(7, -7)).flatten
                }
              }
            }
          }
        }
      }

      if (isEnemyQueenOrKingOrRook(pos + XY(1, 0))) {
        return board.get(pos + XY(1, 0)).flatten
      } else if ((pos + XY(1, 0)).exists && !isPiece(pos + XY(1, 0))) {
        if (isEnemyQueenOrRook(pos + XY(2, 0))) {
          return board.get(pos + XY(2, 0)).flatten
        } else if ((pos + XY(2, 0)).exists && !isPiece(pos + XY(2, 0))) {
          if (isEnemyQueenOrRook(pos + XY(3, 0))) {
            return board.get(pos + XY(3, 0)).flatten
          } else if ((pos + XY(3, 0)).exists && !isPiece(pos + XY(3, 0))) {
            if (isEnemyQueenOrRook(pos + XY(4, 0))) {
              return board.get(pos + XY(4, 0)).flatten
            } else if ((pos + XY(4, 0)).exists && !isPiece(pos + XY(4, 0))) {
              if (isEnemyQueenOrRook(pos + XY(5, 0))) {
                return board.get(pos + XY(5, 0)).flatten
              } else if ((pos + XY(5, 0)).exists && !isPiece(pos + XY(5, 0))) {
                if (isEnemyQueenOrRook(pos + XY(6, 0))) {
                  return board.get(pos + XY(6, 0)).flatten
                } else if ((pos + XY(6, 0)).exists && !isPiece(pos + XY(6, 0))) {
                  if (isEnemyQueenOrRook(pos + XY(7, 0)))
                    return board.get(pos + XY(7, 0)).flatten
                }
              }
            }
          }
        }
      }

      if (isEnemyQueenOrKingOrRook(pos + XY(-1, 0))) {
        return board.get(pos + XY(-1, 0)).flatten
      } else if ((pos + XY(-1, 0)).exists && !isPiece(pos + XY(-1, 0))) {
        if (isEnemyQueenOrRook(pos + XY(-2, 0))) {
          return board.get(pos + XY(-2, 0)).flatten
        } else if ((pos + XY(-2, 0)).exists && !isPiece(pos + XY(-2, 0))) {
          if (isEnemyQueenOrRook(pos + XY(-3, 0))) {
            return board.get(pos + XY(-3, 0)).flatten
          } else if ((pos + XY(-3, 0)).exists && !isPiece(pos + XY(-3, 0))) {
            if (isEnemyQueenOrRook(pos + XY(-4, 0))) {
              return board.get(pos + XY(-4, 0)).flatten
            } else if ((pos + XY(-4, 0)).exists && !isPiece(pos + XY(-4, 0))) {
              if (isEnemyQueenOrRook(pos + XY(-5, 0))) {
                return board.get(pos + XY(-5, 0)).flatten
              } else if ((pos + XY(-5, 0)).exists && !isPiece(pos + XY(-5, 0))) {
                if (isEnemyQueenOrRook(pos + XY(-6, 0))) {
                  return board.get(pos + XY(-6, 0)).flatten
                } else if ((pos + XY(-6, 0)).exists && !isPiece(pos + XY(-6, 0))) {
                  if (isEnemyQueenOrRook(pos + XY(-7, 0)))
                    return board.get(pos + XY(-7, 0)).flatten
                }
              }
            }
          }
        }
      }

      None
    }
  }
}
