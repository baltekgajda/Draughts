package draughtsLogic

import scala.annotation.tailrec

case class Board(size: Int, treeDepth: Int) {

  import Board._

  val boardMatrix: Array[Array[Int]] = initializeBoardMatrix(size)

  //returns true when full move or abort or no possible moves, false when move is not done
  //first boolean - update?, second is game over,
  def performPieceMove(pieceMoveSequence: List[Coord]): (Boolean, Boolean) = {
    val boardMoves = getBoardMoves(boardMatrix, false)
    if (boardMoves.isEmpty) {
      (true, true)
    }
    else if (boardMoves.contains(pieceMoveSequence)) {
      Board.updateBoard(boardMatrix, pieceMoveSequence)
      //oponent move
      val oponentMoveSequence = Minimax(boardMatrix, treeDepth).getOponentMoveSequence //TODO add game over alert
      if (oponentMoveSequence.isEmpty)
        (true, true)
      else {
        Board.updateBoard(boardMatrix, oponentMoveSequence)
        if (Board.isGameOver(boardMatrix)) {
          (true, true)
        }
        else
          (true, false)
      }
    }
    else if (Board.partlyContains(boardMoves, pieceMoveSequence))
      (false, false)
    else {
      (true, false)
    }
  }


}

object Board {

  //get list of possible moves TODO zmienic tego oponenta
  def getBoardMoves(boardMatrix: Array[Array[Int]], isOponent: Boolean): List[List[Coord]] = {
    val killsMovesList = getBoardKillMoves(boardMatrix, isOponent)
    if (killsMovesList.nonEmpty)
      killsMovesList
    else
      getBoardNonKillMoves(boardMatrix, isOponent)
  }

  private def getBoardNonKillMoves(boardMatrix: Array[Array[Int]], isOponent: Boolean): List[List[Coord]] = {

    //returns empty list when cannot move diagonally and move list when move is possible
    def canMoveDiagonally(oldCoords: Coord, newCoords: Coord): List[Coord] = {

      val unitCoord = Coord(newCoords.x - oldCoords.x, newCoords.y - oldCoords.y).normalize()

      @tailrec
      def isTileEmpty(coord: Coord): Boolean = {
        if (coord == newCoords)
          true
        else if (boardMatrix(coord.x)(coord.y) != 0)
          false
        else
          isTileEmpty(coord.add(unitCoord))
      }

      val canMove: Boolean = if (oldCoords.isInsideBoard(boardMatrix.length) && newCoords.isInsideBoard(boardMatrix.length)) {
        //has to be diagonal, newCoords has to be empty
        if ((newCoords.x - oldCoords.x).abs != (newCoords.y - oldCoords.y).abs || boardMatrix(newCoords.x)(newCoords.y) != 0)
          false
        else
          isTileEmpty(oldCoords.add(unitCoord))
      }
      else
        false

      if (canMove)
        List(oldCoords, newCoords)
      else
        List()
    }

    //one if normal piece, two if piece is a king
    val playerStates: (Int, Int) = if (isOponent) (-1, -2) else (1, 2)
    val playerDirection: Int = if (isOponent) 1 else -1

    def getNormalPieceMovesList(pieceCoord: Coord): List[List[Coord]] =
      List(canMoveDiagonally(pieceCoord, pieceCoord.add(Coord(1, playerDirection))),
        canMoveDiagonally(pieceCoord, pieceCoord.add(Coord(-1, playerDirection)))).filter(_.nonEmpty)

    def getKingsMovesList(pieceCoord: Coord): List[List[Coord]] = {

      @tailrec
      def getAllDirectionalMoves(unit: Coord, piece: Coord, list: List[List[Coord]]): List[List[Coord]] = {

        if (!piece.isInsideBoard(boardMatrix.length))
          list.filter(_.nonEmpty)
        else
          getAllDirectionalMoves(unit, piece.add(unit), list ++ List(canMoveDiagonally(pieceCoord, piece.add(unit))))
      }

      getAllDirectionalMoves(Coord(-1, -1), pieceCoord, List()) ++ getAllDirectionalMoves(Coord(-1, 1),
        pieceCoord, List()) ++ getAllDirectionalMoves(Coord(1, -1), pieceCoord, List()) ++ getAllDirectionalMoves(Coord(1, 1), pieceCoord, List())
    }

    val list: List[List[Coord]] = (for (x <- 0 until boardMatrix.length; y <- 0 until boardMatrix.length) yield
      if (boardMatrix(x)(y) == playerStates._1)
        getNormalPieceMovesList(Coord(x, y))
      else if (boardMatrix(x)(y) == playerStates._2) {
        getKingsMovesList(Coord(x, y))
      }
      else
        List()
      ).reduce(_ ++ _)
    list
  }

  private def getBoardKillMoves(boardMatrix: Array[Array[Int]], isOponent: Boolean): List[List[Coord]] = {

    //one if normal piece, two if piece is a king
    val playerStates = if (isOponent) (-1, -2) else (1, 2)
    val oponentStates = if (!isOponent) (-1, -2) else (1, 2)

    //returns empty list when cannot move diagonally and move list when move is possible
    def canKill(oldCoords: Coord, newCoords: Coord): Boolean = {
      val unitCoord = Coord(newCoords.x - oldCoords.x, newCoords.y - oldCoords.y).normalize()

      @tailrec
      //todo dodac wartosci domyslne w niektorych
      def isOnePieceToKill(coord: Coord, killPieceFound: Boolean = false): Boolean = {
        if (coord == newCoords)
          killPieceFound
        else {
          boardMatrix(coord.x)(coord.y) match {
            case oponentStates._1 | oponentStates._2 =>
              if (!killPieceFound)
                isOnePieceToKill(coord.add(unitCoord), !killPieceFound)
              else
                false
            case playerStates._1 | playerStates._2 => false
            case 0 => isOnePieceToKill(coord.add(unitCoord), killPieceFound)
          }
        }
      }

      if (oldCoords.isInsideBoard(boardMatrix.length) && newCoords.isInsideBoard(boardMatrix.length)) {
        //has to be diagonal, newCoords has to be empty
        if ((newCoords.x - oldCoords.x).abs != (newCoords.y - oldCoords.y).abs || boardMatrix(newCoords.x)(newCoords.y) != 0)
          false
        else
          isOnePieceToKill(oldCoords.add(unitCoord))
      }
      else
        false
    }

    //todo co jak argument jest krotszy od 2
    //returns reversed sequence - can contain kills x,y and x,y,z
    def enlargeKillSequence(revertedSeq: List[Coord]): List[List[Coord]] = {
      if (revertedSeq.length >= 2 && !revertedSeq.tail.contains(revertedSeq.head) && canKill(revertedSeq.tail.head, revertedSeq.head)) {
        val previousDirection = revertedSeq.head.subtract(revertedSeq.tail.head).normalize()
        val sequences = Coord.getOtherDirectionUnitVectors(previousDirection.negate)
          .map(coord => revertedSeq.head.add(coord).add(coord) :: revertedSeq)
          .map(seq => enlargeKillSequence(seq))
          .distinct
          .map(seq => if (seq.isEmpty) List(revertedSeq) else seq)
          .reduce(_ ++ _)
        val maxLength = sequences.maxBy(_.length).length //only longest kill sequences
        sequences.filter(_.length >= maxLength)
      }
      else
        List()
    }

    def getNormalPieceKillMovesList(pieceCoord: Coord): List[List[Coord]] = {
      Coord.getAllDirectionUnitVectors
        .map(coord => List(pieceCoord.add(coord).add(coord), pieceCoord))
        .map(seq => enlargeKillSequence(seq))
        .distinct
        .map(seq => if (seq.isEmpty) List() else seq)
        .reduce(_ ++ _)
        .map(_.reverse)
    }

    def getKingsKillMovesList(pieceCoord: Coord): List[List[Coord]] = {
      getNormalPieceKillMovesList(pieceCoord)
    }

    (for (x <- 0 until boardMatrix.length; y <- 0 until boardMatrix.length) yield
      if (boardMatrix(x)(y) == playerStates._1)
        getNormalPieceKillMovesList(Coord(x, y))
      else if (boardMatrix(x)(y) == playerStates._2)
        getKingsKillMovesList(Coord(x, y))
      else
        List()
      ).reduce(_ ++ _)
  }

  def partlyContains(boardMoves: List[List[Coord]], moveSequence: List[Coord]): Boolean = {

    @tailrec
    def ifListPartlyContains(list: List[List[Coord]]): Boolean = {
      if (list.isEmpty)
        false
      else {
        if (list.head.containsSlice(moveSequence))
          true
        else
          ifListPartlyContains(list.tail)
      }
    }

    ifListPartlyContains(boardMoves)
  }

  def updateBoard(boardMatrix: Array[Array[Int]], moveSequence: List[Coord]): Unit = {

    val pieceCoord = boardMatrix(moveSequence.head.x)(moveSequence.head.y)

    @tailrec
    def removePiece(deleteCoord: Coord, endCoord: Coord, unitCoord: Coord): Unit = {
      if (deleteCoord != endCoord) {
        boardMatrix(deleteCoord.x)(deleteCoord.y) = 0
        removePiece(deleteCoord.add(unitCoord), endCoord, unitCoord)
      }
    }

    @tailrec
    def updateTiles(moveSeq: List[Coord]): Unit = {
      if (moveSeq.length > 1) {
        val unitCoord = Coord(moveSeq.tail.head.x - moveSeq.head.x, moveSeq.tail.head.y - moveSeq.head.y).normalize()
        removePiece(moveSeq.head, moveSeq.tail.head, unitCoord)
        updateTiles(moveSeq.tail)
      }
    }

    updateTiles(moveSequence)

    //change piece to king
    if (moveSequence.last.y == 0 && pieceCoord == 1)
      boardMatrix(moveSequence.last.x)(moveSequence.last.y) = 2
    else if (moveSequence.last.y == boardMatrix.length - 1 && pieceCoord == -1)
      boardMatrix(moveSequence.last.x)(moveSequence.last.y) = -2
    else
      boardMatrix(moveSequence.last.x)(moveSequence.last.y) = pieceCoord
  }

  def isGameOver(boardMatrix: Array[Array[Int]]): Boolean = {
    val initial = (0, 0)
    val minMax = (for (x <- 0 until boardMatrix.length; y <- 0 until boardMatrix.length) yield boardMatrix(x)(y)).foldLeft(initial) { (acc, x) =>
      if (x < acc._1) (x, acc._2)
      else if (x > acc._2) (acc._1, x)
      else acc
    }
    if (minMax._1 == 0 || minMax._2 == 0)
      true
    else false
  }

  def getPiecesCount(boardMatrix: Array[Array[Int]], isOponent: Boolean): Int = {
    val initial = (0, 0)
    val pieceCount = (for (x <- 0 until boardMatrix.length; y <- 0 until boardMatrix.length) yield boardMatrix(x)(y)).foldLeft(initial) { (acc, x) =>
      if (x < 0) (acc._1 + 1, acc._2)
      else if (x > 0) (acc._1, acc._2 + 1)
      else acc
    }

    if (isOponent)
      pieceCount._1
    else
      pieceCount._2
  }

  def copyBoard(board: Array[Array[Int]]): Array[Array[Int]] = {
    (for (x <- 0 until board.length) yield
      (for (y <- 0 until board.length) yield
        board(x)(y)
        ).toArray
      ).toArray
  }

  private def initializeBoardMatrix(size: Int): Array[Array[Int]] = {
    (for (x <- 0 until size) yield //TODO -1 zamienic na klase enum
      (for (y <- 0 until size) yield
        if (y < ((size - 2) / 2) && (x + y) % 2 != 0)
          -1
        else if (y > ((size - 2) / 2 + 1) && (x + y) % 2 != 0)
          1
        else
          0
        ).toArray
      ).toArray
  }
}
