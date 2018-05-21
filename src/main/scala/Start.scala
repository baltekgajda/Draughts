
import draughtsLogic.Board
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene

object Start extends JFXApp {

  stage = new PrimaryStage {
    title = "Draughts"
    scene = new Scene() {
      val draughtsBoard = new Board
      content = draughtsBoard.renderEmptyBoard()
      draughtsBoard.addPiecesToBoard()
    }
  }
}