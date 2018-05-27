package draughtsLogic

import scalafx.Includes._
import scalafx.scene.Group
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, ButtonType}
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

import scala.annotation.tailrec

class Board {

  import Board._

  private val boardMatrix = initializeBoardMatrix()
  private val piecesGroup = new Group()

  def renderEmptyBoard(): Pane = {
    new Pane {
      children = for (x <- 0 until BOARD_SIZE; y <- 0 until BOARD_SIZE) yield createTile(x, y)
      children.add(piecesGroup)
    }
  }

  def addPiecesToBoard(): Unit = {
    piecesGroup.children = (for (y <- 0 until BOARD_SIZE; x <- 0 until BOARD_SIZE) yield {
      val piece: Piece = new Piece(x, y) {

        onMouseReleased = (e: MouseEvent) => {
          val mouseCoord = Coord((e.getSceneX / TILE_SIZE).toInt, (e.getSceneY / TILE_SIZE).toInt)
          if (!performPieceMove(getMoveSequence :+ mouseCoord)) {
            addToMoveSequence(mouseCoord)
            setPosition(mouseCoord)
          }
        }
      }

      boardMatrix(x)(y) match {
        case -2 => Some(piece.setColor(true, true))
        case -1 => Some(piece.setColor(true, false))
        case 1 => Some(piece.setColor(false, false))
        case 2 => Some(piece.setColor(false, true))
        case 0 => None
      }
    }).flatten
  }

  private def endGameAlert(): Unit = {
    val result = new Alert(AlertType.Confirmation) {
      title = "Draughts"
      headerText = "Game over."
      contentText = "Start a new game?"
    }.showAndWait()

    result match {
      case Some(ButtonType.OK) => {
        val startBoardMatrix = initializeBoardMatrix()
        for (x <- 0 until BOARD_SIZE; y <- 0 until BOARD_SIZE) yield boardMatrix(x)(y) = startBoardMatrix(x)(y)
        addPiecesToBoard()
      }
      case _ => System.exit(0);
    }
  }

  //returns true when full move or abort or no possible moves, false when move is not done
  private def performPieceMove(pieceMoveSequence: List[Coord]): Boolean = {
    val boardMoves = Board.getBoardMoves(boardMatrix, false)
    if (boardMoves.isEmpty) {
      endGameAlert()
      true
    }
    else if (boardMoves.contains(pieceMoveSequence)) {
      println("perform full move")
      updateBoard(boardMatrix, pieceMoveSequence)
      addPiecesToBoard()

      //oponent move
      val oponentMoveSequence = Minimax(boardMatrix, 5).getOponentMoveSequence //TODO add game over alert
      updateBoard(boardMatrix, oponentMoveSequence)
      addPiecesToBoard()
      true
    }
    else if (partlyContains(boardMoves, pieceMoveSequence)) {
      println("partly contains move")
      false
    }
    else {
      println("abort move")
      addPiecesToBoard()
      true
    }
  }
  //delete all pieces that are killed,
}

object Board {

  val BOARD_SIZE = 8
  val TILE_SIZE = 50

  //get list of possible moves
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
        if (coord == newCoords.add(unitCoord))
          true
        else if (boardMatrix(coord.x)(coord.y) != 0)
          false
        else
          isTileEmpty(coord.add(unitCoord))
      }

      val canMove: Boolean = if (oldCoords.isInsideBoard(BOARD_SIZE) && newCoords.isInsideBoard(BOARD_SIZE)) {
        //has to be diagonal, newCoords has to be empty
        if ((newCoords.x - oldCoords.x).abs != (newCoords.y - oldCoords.y).abs || boardMatrix(newCoords.x)(newCoords.y) != 0)
          false
        else {
          isTileEmpty(oldCoords.add(unitCoord))
        }
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

        if (!piece.isInsideBoard(BOARD_SIZE))
          list.filter(_.nonEmpty)
        else
          getAllDirectionalMoves(unit, piece.add(unit), list ++ List(canMoveDiagonally(pieceCoord, piece.add(unit))))
      }

      getAllDirectionalMoves(Coord(-1, -1), pieceCoord, List()) ++ getAllDirectionalMoves(Coord(-1, 1),
        pieceCoord, List()) ++ getAllDirectionalMoves(Coord(1, -1), pieceCoord, List()) ++ getAllDirectionalMoves(Coord(1, 1), pieceCoord, List())
    }

    val list: List[List[Coord]] = (for (x <- 0 until BOARD_SIZE; y <- 0 until BOARD_SIZE) yield
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

  //TODO
  private def getBoardKillMoves(boardMatrix: Array[Array[Int]], isOponent: Boolean): List[List[Coord]] = {

    //returns empty list when cannot move diagonally and move list when move is possible
    def canKill(oldCoords: Coord, newCoords: Coord): List[Coord] = {

      val unitCoord = Coord(newCoords.x - oldCoords.x, newCoords.y - oldCoords.y).normalize()

      @tailrec
      def isTileEmpty(coord: Coord): Boolean = {
        if (coord == newCoords.add(unitCoord))
          true
        else if (boardMatrix(coord.x)(coord.y) != 0)
          false
        else
          isTileEmpty(coord.add(unitCoord))
      }

      val canMove: Boolean = if (oldCoords.isInsideBoard(BOARD_SIZE) && newCoords.isInsideBoard(BOARD_SIZE)) {
        //has to be diagonal, newCoords has to be empty
        if ((newCoords.x - oldCoords.x).abs != (newCoords.y - oldCoords.y).abs || boardMatrix(newCoords.x)(newCoords.y) != 0)
          false
        else {
          isTileEmpty(oldCoords.add(unitCoord))
        }
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

    def getNormalPieceKillMovesList(pieceCoord: Coord): List[List[Coord]] =
      List(canKill(pieceCoord, pieceCoord.add(Coord(2, 2 * playerDirection))),
        canKill(pieceCoord, pieceCoord.add(Coord(-2, 2 * playerDirection)))).filter(_.nonEmpty)

    def getKingsKillMovesList(pieceCoord: Coord): List[List[Coord]] = {

      @tailrec
      def getAllDirectionalKillMoves(unit: Coord, piece: Coord, list: List[List[Coord]]): List[List[Coord]] = {

        if (!piece.isInsideBoard(BOARD_SIZE))
          list.filter(_.nonEmpty)
        else
          getAllDirectionalKillMoves(unit, piece.add(unit), list ++ List(canKill(pieceCoord, piece.add(unit))))
      }

      getAllDirectionalKillMoves(Coord(-2, -2), pieceCoord, List()) ++ getAllDirectionalKillMoves(Coord(-2, 2),
        pieceCoord, List()) ++ getAllDirectionalKillMoves(Coord(2, -2), pieceCoord, List()) ++ getAllDirectionalKillMoves(Coord(2, 2), pieceCoord, List())
    }

    val list: List[List[Coord]] = (for (x <- 0 until BOARD_SIZE; y <- 0 until BOARD_SIZE) yield
      if (boardMatrix(x)(y) == playerStates._1)
        getNormalPieceKillMovesList(Coord(x, y))
      else if (boardMatrix(x)(y) == playerStates._2) {
        getKingsKillMovesList(Coord(x, y))
      }
      else
        List()
      ).reduce(_ ++ _)
    list
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

    val pieceValue = boardMatrix(moveSequence.head.x)(moveSequence.head.y)

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
    if (moveSequence.last.y == 0 && pieceValue == 1)
      boardMatrix(moveSequence.last.x)(moveSequence.last.y) = 2
    else if (moveSequence.last.y == BOARD_SIZE - 1 && pieceValue == -1)
      boardMatrix(moveSequence.last.x)(moveSequence.last.y) = -2
    else
      boardMatrix(moveSequence.last.x)(moveSequence.last.y) = pieceValue
  }

  def isGameOver(boardMatrix: Array[Array[Int]]): Boolean = {
    val initial = (0, 0)
    val minMax = (for (x <- 0 until BOARD_SIZE; y <- 0 until BOARD_SIZE) yield boardMatrix(x)(y)).foldLeft(initial) { (acc, x) =>
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
    val pieceCount = (for (x <- 0 until BOARD_SIZE; y <- 0 until BOARD_SIZE) yield boardMatrix(x)(y)).foldLeft(initial) { (acc, x) =>
      if (x < 0) (acc._1 + 1, acc._2)
      else if (x > 0) (acc._1, acc._2 + 1)
      else acc
    }

    if (isOponent)
      pieceCount._1
    else
      pieceCount._2
  }

  //creates matrix for a new game
  private def initializeBoardMatrix(): Array[Array[Int]] = {
    (for (x <- 0 until BOARD_SIZE) yield
      (for (y <- 0 until BOARD_SIZE) yield
        if (y < ((BOARD_SIZE - 2) / 2) && (x + y) % 2 != 0)
          -1
        else if (y > ((BOARD_SIZE - 2) / 2 + 1) && (x + y) % 2 != 0)
          1
        else
          0
        ).toArray
      ).toArray
  }

  //returns rectangular tile with proper color and coordinates
  private def createTile(_x: Int, _y: Int): Rectangle = {
    val color = if ((_x + _y) % 2 == 0) Color.White
    else Color.Gray
    new Rectangle {
      x = (_x * TILE_SIZE).toDouble
      y = (_y * TILE_SIZE).toDouble
      width = TILE_SIZE
      height = TILE_SIZE
      fill = color
    }
  }

  def copyBoard(board: Array[Array[Int]]): Array[Array[Int]] = {
    (for (x <- 0 until BOARD_SIZE) yield
      (for (y <- 0 until BOARD_SIZE) yield
        board(x)(y)
        ).toArray
      ).toArray
  }
}


