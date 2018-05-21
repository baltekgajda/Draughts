package draughtsLogic

import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle
import scalafx.Includes._

class Piece(x: Double, y: Double, isOponent: Boolean, isKing: Boolean) extends Circle {

  import Board.TILE_SIZE

  radius = 0.3 * TILE_SIZE
  centerX = TILE_SIZE * (x + 0.5)
  centerY = TILE_SIZE * (y + 0.5)

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

  onMouseDragged = (e: MouseEvent) => {
    centerX = e.getSceneX
    centerY = e.getSceneY
  }

  onMouseReleased = (e: MouseEvent) => {
    println("Pressed")    //TODO
  }

  //def getCoord(): (Int, Int) = (centerX.toInt, centerY.toInt) TODO

}
