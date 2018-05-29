package draughtsLogic

import scala.math.abs

case class Coord(x: Int, y: Int) {

  def isInsideBoard(boardSize: Int): Boolean = {
    if (x < 0 || x >= boardSize || y < 0 || y >= boardSize)
      false
    else
      true
  }

  def normalize(): Coord =
    Coord(if (x != 0) x / abs(x) else 0, if (y != 0) y / abs(y) else 0)

  def add(coord: Coord): Coord =
    Coord(x + coord.x, y + coord.y)

  def subtract(coord: Coord): Coord =
    Coord(x - coord.x, y - coord.y)

  def negate: Coord =
    Coord(x * -1, y * -1)
}

object Coord {

  def getOtherDirectionUnitVectors(unit: Coord): List[Coord] =
    getAllDirectionUnitVectors.filter(_ != unit)

  def getAllDirectionUnitVectors: List[Coord] =
    List(Coord(-1, -1), Coord(-1, 1), Coord(1, -1), Coord(1, 1))
}
