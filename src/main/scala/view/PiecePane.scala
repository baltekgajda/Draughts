package view

import controller.Controller
import draughtsLogic.{Board, Coord}
import scalafx.Includes._
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout.Pane

case class PiecePane(boardSize: Int, tileSize: Double, board: Board, gameScene: GameScene) extends Pane {

  children = (for (y <- 0 until boardSize; x <- 0 until boardSize) yield {
    if (board.boardMatrix(x)(y) != 0) {
      val piece = new Piece(x, y, tileSize, board.boardMatrix(x)(y)) {
        onMouseReleased = (e: MouseEvent) => {
          val mouseCoord = Coord((e.getX / tileSize).toInt, (e.getY / tileSize).toInt)
          if (!Controller.performFullMove(getMoveSequence :+ mouseCoord, board, gameScene)) {
            addToMoveSequence(mouseCoord)
            setPosition(mouseCoord)
          }
        }
      }
      Some(piece)
    }
    else
      None
  }).flatten

  this.setPrefHeight(boardSize * tileSize)
  this.setPrefWidth(boardSize * tileSize)
  this.styleClass = List("board-pane")
}