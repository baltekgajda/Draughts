package draughtsLogic

import scalafx.Includes._
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle

class Piece(private val x: Int, private val y: Int) extends Circle {

  import Board.TILE_SIZE

  radius = 0.3 * TILE_SIZE
  centerX = TILE_SIZE * (x + 0.5)
  centerY = TILE_SIZE * (y + 0.5)

  onMouseDragged = (e: MouseEvent) => {
    centerX = e.getSceneX
    centerY = e.getSceneY
  }

  //set piece color
  def setColor(isOponent: Boolean, isKing: Boolean): this.type = {
    (isOponent, isKing) match {
      case (false, false) => fill = Color.White
      case (false, true) => fill = Color.White
        strokeWidth = 5.0
        stroke = Color.Black
      case (true, false) => fill = Color.Black
      case (true, true) => fill = Color.Black
        strokeWidth = 5.0
        stroke = Color.White
    }
    this
  }

  //return piece coordinates before mouse events
  def getCoord: Coord = Coord(x, y)

}