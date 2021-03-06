package controller

import draughtsLogic.{Board, BoardSize, Coord, Difficulty}
import start.Start.startApp
import view.{GameScene, MenuScene}

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

  def loadNewGame(boardSize: BoardSize.Value, difficulty: Difficulty.Value): Unit = {
    startApp.stage.scene = GameScene(600, BoardSize.getBoardSize(boardSize), Difficulty.getTreeDepth(difficulty))
  }

  def loadNewGame(gameScene: GameScene): Unit = {
    gameScene.updatePieces(Board(gameScene.boardSize, gameScene.treeDepth))
  }

  def exitDraughts(): Unit = {
    System.exit(0)
  }

  def returnToMainMenu(): Unit = {
    startApp.stage.scene = MenuScene(600, 600)
  }
}
