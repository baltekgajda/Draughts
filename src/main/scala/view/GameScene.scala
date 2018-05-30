package view

import draughtsLogic.Board
import scalafx.scene.Scene
import scalafx.scene.layout.{FlowPane, Pane}
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

class GameScene(windowWidth: Double, windowHeight: Double, boardSize: Int = 8) extends Scene {

  val draughtsBoard = new Board

  fill = Color.BlanchedAlmond
  content = renderEmptyBoard
  //draughtsBoard.addPiecesToBoard()
  val k = new FlowPane {
    /*        layoutX_=(windowWidth/2-300)
            layoutY_=(windowHeight/2-300)*/
    prefHeight_=(600)
    prefWidth_=(600)
    children = for (x <- 0 until boardSize; y <- 0 until boardSize) yield createTile(x, y, 60)
  }

  private def renderEmptyBoard: Pane = {
    new Pane {

      children = k

      //  children.add(piecesGroup)
    }
  }

  private def createTile(_x: Int, _y: Int, size: Int): Rectangle = {
    new Rectangle {
      width = size
      height = size
      fill = if ((_x + _y) % 2 == 0) Color.White
      else Color.Gray
    }
  }

}
