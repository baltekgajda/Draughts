package draughtsLogic

object Difficulty extends Enumeration {
  type Difficulty = Value
  val EASY, MEDIUM, HARD = Value

  def getTreeDepth(value: Value): Int = {
    value match {
      case EASY => 1
      case MEDIUM => 3
      case HARD => 5
      case _ => 0
    }
  }
}