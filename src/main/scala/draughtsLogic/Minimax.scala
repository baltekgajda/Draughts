package draughtsLogic

import scala.annotation.tailrec
import scala.math.{max, min}
import scala.util.Random

case class Minimax(boardMatrix: Array[Array[Int]], maxDepth: Int) {
  def getOponentMoveSequence: List[Coord] = {
    //TODO dodac jak nie ma mozliwych ruchów
    if (!Board.isGameOver(boardMatrix)) {
      val movesToChooseFrom = Board.getBoardMoves(boardMatrix, true)
      if (movesToChooseFrom.isEmpty)
        List() //TODO alert konca gry
      else
        Random.shuffle(movesToChooseFrom).map(l => {
          val newBoard = Board.copyBoard(boardMatrix)
          Board.updateBoard(newBoard, l)
          (l, minimaxAlphaBeta(newBoard, false, maxDepth - 1, Int.MinValue, Int.MaxValue))
        }).maxBy(_._2)._1
    }
    else
      List() //TODO alert konca gry
  }

  //TODO psuje sie gdy nie ma mozliwosci ruchu
  //TODO dodac klasy graczy
  //maksymalizujacy gracz to musi byc robot

  def minimaxAlphaBeta(board: Array[Array[Int]], maximizingPlayer: Boolean, depth: Int, alpha: Int, beta: Int): Int = {

    @tailrec
    def visitMaxMove(list: List[List[Coord]], a: Int, b: Int, maxEval: Int): Int = {
      if (list.isEmpty || b <= a)
        maxEval
      else {
        val newBoard = Board.copyBoard(board)
        Board.updateBoard(newBoard, list.head)
        val eval = minimaxAlphaBeta(newBoard, !maximizingPlayer, depth - 1, a, b)
        visitMaxMove(list.tail, max(a, eval), b, max(maxEval, eval))
      }
    }

    @tailrec
    def visitMinMove(list: List[List[Coord]], a: Int, b: Int, minEval: Int): Int = {
      if (list.isEmpty || b <= a)
        minEval
      else {
        val newBoard = Board.copyBoard(board)
        Board.updateBoard(newBoard, list.head)
        val eval = minimaxAlphaBeta(newBoard, !maximizingPlayer, depth - 1, a, b)
        visitMinMove(list.tail, a, min(b, eval), min(minEval, eval))
      }
    }

    //TODO co jak nie ma ruchow do zrobienia?
    if (depth == 0 || Board.isGameOver(board))
      calcHeuristicValue(board)
    else {
      val possibleMoves = Board.getBoardMoves(board, maximizingPlayer)
      if (maximizingPlayer)
        visitMaxMove(possibleMoves, alpha, beta, Int.MinValue)
      else
        visitMinMove(possibleMoves, alpha, beta, Int.MaxValue)
    }
  }

  private def calcHeuristicValue(board: Array[Array[Int]]): Int = {
    //TODO dodac damki w heurystyce
    Board.getPiecesCount(board, true) - Board.getPiecesCount(board, false)
  }
}
