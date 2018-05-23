package draughtsLogic

import scalafx.Includes._
import scalafx.scene.Group
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

import scala.annotation.tailrec

class Board {

  import Board._

  private val boardMatrix = initializeBoardMatrix()
  private val piecesGroup = new Group()

  //TODO to delete
  var isOp: Boolean = false

  def renderEmptyBoard(): Pane = {
    new Pane {
      children = for (x <- 0 until BOARD_SIZE; y <- 0 until BOARD_SIZE) yield createTile(x, y)
      children.add(piecesGroup)
    }
  }

  def addPiecesToBoard(): Unit = {
    piecesGroup.children = (for (x <- 0 until BOARD_SIZE; y <- 0 until BOARD_SIZE) yield {
      val piece: Piece = new Piece(x, y) {

        onMouseReleased = (e: MouseEvent) => {
          val mouseCoord = Coord((e.getSceneX / TILE_SIZE).toInt, (e.getSceneY / TILE_SIZE).toInt)
          performPieceMove(getMoveSequence :+ mouseCoord)
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

  private def performPieceMove(pieceMoveSequence: List[Coord]): Unit = {
    val boardMoves = Board.getBoardMoves(boardMatrix, isOp)
    if (isOp)
      isOp = false
    else
      isOp = true

    if (boardMoves.isEmpty) {
      //TODO what happens when there is no possible moves
      addPiecesToBoard()
      println("no possible moves")
    }
    else if (boardMoves.contains(pieceMoveSequence)) {
      println("perform full move")
      updateBoard(pieceMoveSequence)
      addPiecesToBoard()
    }
    else if (partlyContains(boardMoves, pieceMoveSequence)) {
      //TODO continue kill
      println("partly contains move")
    }
    else {
      println("abort move")
      addPiecesToBoard()
    }
  }

  //delete all pieces that are killed,
  private def updateBoard(moveSequence: List[Coord]): Unit = {

    val pieceValue = boardMatrix(moveSequence.head.x)(moveSequence.head.y)

    @tailrec
    def updateTile(moveSeq: List[Coord]): Unit = {
      if (moveSeq.length > 1) {
        boardMatrix(moveSeq.head.x)(moveSeq.head.y) = 0
        updateTile(moveSeq.tail)
      }
    }

    updateTile(moveSequence)
    boardMatrix(moveSequence.last.x)(moveSequence.last.y) = pieceValue
  }
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

  //TODO
  private def getBoardNonKillMoves(boardMatrix: Array[Array[Int]], isOponent: Boolean): List[List[Coord]] = {

    def canMoveDiagonally(oldCoords: Coord, newCoords: Coord): Boolean = {

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

      if (oldCoords.isInsideBoard(BOARD_SIZE) && newCoords.isInsideBoard(BOARD_SIZE)) {
        //has to be diagonal, newCoords has to be empty
        if ((newCoords.x - oldCoords.x).abs != (newCoords.y - oldCoords.y).abs || boardMatrix(newCoords.x)(newCoords.y) != 0)
          false
        else {
          isTileEmpty(oldCoords.add(unitCoord))
        }
      }
      else
        false
    }

    //one if normal piece, two if piece is a king
    val playerStates: (Int, Int) = if (isOponent) (-1, -2) else (1, 2)
    val playerDirection: Int = if (isOponent) 1 else -1

    //returns list of for instance possible left moves
    def getDirectionalMovesList(xTranslation: Int, yTranslation: Int, playerState: Int): List[List[Coord]] = {
      (for (x <- 0 until BOARD_SIZE; y <- 0 until BOARD_SIZE) yield {
        if (boardMatrix(x)(y) == playerState) {
          if (canMoveDiagonally(Coord(x, y), Coord(x + xTranslation, y + playerDirection * yTranslation)))
            Some(List(Coord(x, y), Coord(x + xTranslation, y + playerDirection * yTranslation)))
          else
            None
        } else
          None
      }
        ).flatten.filter(_.nonEmpty).toList
    }

    def getKingsMovesList(playerState: Int): List[List[Coord]] = {
      //na x: od -(BOARD_SIZE-1) do (BOARD_SIZE-1)
      //na y to samo 
      List[List[Coord]]()
    }

    val normalPiecesMoves = getDirectionalMovesList(1, 1, playerStates._1) ++ getDirectionalMovesList(-1, 1, playerStates._1)
    val kingPiecesMoves = getKingsMovesList(playerStates._2)
    normalPiecesMoves ++ kingPiecesMoves
  }

  private def getBoardKillMoves(boardMatrix: Array[Array[Int]], isOponent: Boolean): List[List[Coord]]

  = {
    //TODO
    List[List[Coord]]()
  }

  def partlyContains(boardMoves: List[List[Coord]], moveSequence: List[Coord]): Boolean = {
    //TODO
    false
  }

  //creates matrix for a new game
  private def initializeBoardMatrix(): Array[Array[Int]]

  = {
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

}


