package draughtsLogic

import scalafx.Includes._
import scalafx.scene.Group
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.Pane
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

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

  def performPieceMove(pieceMoveSequence: List[Coord]): Unit = {
    val boardMoves = Board.getBoardMoves(boardMatrix, false)
    if (boardMoves.isEmpty) {
      //TODO what happens when there is no possible moves
      println("no possible moves")
    }
    else if (boardMoves.contains(pieceMoveSequence)) {
      //TODO add when move is not full - there is still one or more kills to perform
      println("successful full move")
    }
    else if (partlyContains(boardMoves, pieceMoveSequence)) {
      //TODO continue kill
      println("partly contains")
    }
    else {
      println("abort move")
      addPiecesToBoard()
    }
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
      getBoardNotKillMoves(boardMatrix, isOponent)
  }

  private def getBoardNotKillMoves(boardMatrix: Array[Array[Int]], isOponent: Boolean): List[List[Coord]] = {
    //TODO
    val list: List[Coord] = List[Coord]() :+ Coord(1, 2) :+ Coord(1, 4)
    println(list)
    List[List[Coord]]() :+ list
  }

  private def getBoardKillMoves(boardMatrix: Array[Array[Int]], isOponent: Boolean): List[List[Coord]] = {
    //TODO
    List[List[Coord]]()
  }

  def partlyContains(boardMoves: List[List[Coord]], moveSequence: List[Coord]): Boolean = {
    //TODO
    false
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

}


