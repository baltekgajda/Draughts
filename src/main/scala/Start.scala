
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.stage.StageStyle
import view.GameScene

object Start extends JFXApp {
  stage = new PrimaryStage {
    title = "Draughts"
    height = 640
    width = 1280
    centerOnScreen()
    initStyle(StageStyle.Decorated)
    resizable = false
    scene = new GameScene(width.value, 640, 10)
  }
}