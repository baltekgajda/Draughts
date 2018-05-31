package start

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import view.GameScene

object Start extends JFXApp {
  stage = new PrimaryStage {
    title = "Draughts"
    centerOnScreen()
    sizeToScene()
    resizable = false
    scene = new GameScene(600) //TODO zmienic gdy boardSize = 4
  }
}