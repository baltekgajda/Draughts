package view

import draughtsLogic.Coord
import scalafx.Includes._
import scalafx.scene.input.MouseEvent
import scalafx.scene.paint.Color
import scalafx.scene.shape.Circle

import scala.collection.mutable.ListBuffer

case class Piece(_x: Int, _y: Int, tileSize: Double, color: Int) extends Circle {

  radius = 0.3 * tileSize
  setPosition(Coord(_x, _y))
  //TODO dodac enum dla typu
  color match {
    case -2 => setColor(true, true)
    case -1 => setColor(true, false)
    case 1 => setColor(false, false)
    case 2 => setColor(false, true)
    case 0 => visible = false
  }

  val moveSequence = ListBuffer(Coord(_x, _y))
  onMouseDragged = (e: MouseEvent) => {
    centerX = e.getX
    centerY = e.getY
  }

  //TODO dodac typy graczy
  def setColor(isOponent: Boolean, isKing: Boolean): Unit = {
    (isOponent, isKing) match {
      case (false, false) => fill = Color.Brown
      case (false, true) => fill = Color.Brown
        strokeWidth = 5.0
        stroke = Color.Black
      case (true, false) => fill = Color.Black
      case (true, true) => fill = Color.Black
        strokeWidth = 5.0
        stroke = Color.Brown
    }
  }

  //returns current move sequence as immutable list
  def getMoveSequence: List[Coord] = moveSequence.toList

  def addToMoveSequence(coord: Coord): Unit = moveSequence += coord

  def setPosition(coord: Coord): Unit = {
    centerX = tileSize * (coord.x + 0.5)
    centerY = tileSize * (coord.y + 0.5)
  }
}