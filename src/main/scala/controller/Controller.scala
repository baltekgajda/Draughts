package controller

import draughtsLogic.{Board, Coord}
import view.GameScene

object Controller {

  def performFullMove(pieceMoveSequence: List[Coord], board: Board, gameScene: GameScene): Boolean = {
    val result = board.performPieceMove(pieceMoveSequence)
    if (result._1)
      gameScene.updatePieces(board)
    if (result._2)
      endGameAlert(gameScene)
    if (result == (false, false))
      false
    else
      true
  }

  def endGameAlert(gameScene: GameScene): Unit = {
    gameScene.showEndGameAlert()
  }

  def returnToMainMenu(): Unit = {
    println("Go to main menu")
  }

  def loadNewGame(gameScene: GameScene): Unit = {
    gameScene.updatePieces(Board(gameScene.boardSize))
  }
}
