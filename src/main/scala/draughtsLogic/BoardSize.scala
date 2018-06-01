package draughtsLogic

object BoardSize extends Enumeration {
  type BoardSize = Value
  val SMALL, MEDIUM, LARGE = Value

  def getBoardSize(value: Value): Int = {
    value match {
      case SMALL => 8
      case MEDIUM => 10
      case LARGE => 12
      case _ => 14
    }
  }
}