package view

import controller.Controller
import scalafx.Includes._
import scalafx.geometry.Pos
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout._
import scalafx.scene.text.Text

case class MenuScene(sceneWidth: Double, sceneHeight: Double) extends Scene {

  private val titlePane: Text = new Text {
    this.text = "Draughts"
    this.styleClass = List("text-style")
  }

  private val buttonsVBox: VBox = {
    val vbox = new VBox {
      this.spacing = 20
      this.alignment = Pos.Center
      val newGameButton: Button = GameScene.createButton(120, 69, "New Game")
      val exitButton: Button = GameScene.createButton(120, 69, "Exit")
      this.children = List(titlePane, newGameButton, exitButton)
    }
    vbox.newGameButton.onMouseReleased =
      (e: MouseEvent) => Controller.loadNewGame()
    vbox.exitButton.onMouseReleased =
      (e: MouseEvent) => Controller.exitDraughts()
    vbox
  }

  private val sceneStackPane: StackPane = new StackPane {
    this.styleClass = List("menu-scene")
    this.minHeight = sceneHeight
    this.minWidth = sceneWidth
    this.children = buttonsVBox
  }

  stylesheets = List(getClass.getClassLoader.getResource("styles.css").toExternalForm)
  content = sceneStackPane
}
