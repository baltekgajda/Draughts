package start

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.image.Image
import view.MenuScene

object Start extends JFXApp {
  def startApp: JFXApp = this
  stage = new PrimaryStage {
    title = "Draughts"
    icons.add(new Image(getClass.getResourceAsStream("/images/DraughtsLogo16X16.png")))
    centerOnScreen()
    sizeToScene()
    resizable = false
    scene = MenuScene(600, 600)
  }
}