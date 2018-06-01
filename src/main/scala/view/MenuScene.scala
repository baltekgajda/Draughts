package view

import controller.Controller
import draughtsLogic.{BoardSize, Difficulty}
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

  private val mainMenuButtons: List[Button] = {
    val newGameButton: Button = GameScene.createButton(250, 45, "Start Game")
    val difficultyButton: Button = GameScene.createButton(200, 45, "Set difficulty: Easy")
    val sizeButton: Button = GameScene.createButton(250, 45, "Set size: 8x8")
    val exitButton: Button = GameScene.createButton(200, 45, "Exit")

    newGameButton.onMouseReleased =
      (e: MouseEvent) =>
        Controller.loadNewGame(stringToBoardSize(sizeButton.text.value.split(" ").last),
          stringToDifficulty(difficultyButton.text.value.split(" ").last))

    difficultyButton.onMouseReleased =
      (e: MouseEvent) => difficultyButton.setText(
        difficultyButton.text.value.split(" ").last.toLowerCase match {
          case "easy" => "Set difficulty: Medium"
          case "medium" => "Set difficulty: Hard"
          case "hard" => "Set difficulty: Easy"
        }
      )

    sizeButton.onMouseReleased =
      (e: MouseEvent) => sizeButton.setText(
        sizeButton.text.value.split(" ").last match {
          case "8x8" => "Set size: 10x10"
          case "10x10" => "Set size: 12x12"
          case "12x12" => "Set size: 8x8"
        }
      )

    exitButton.onMouseReleased =
      (e: MouseEvent) => Controller.exitDraughts()

    List(newGameButton, difficultyButton, sizeButton, exitButton)
  }

  private val buttonsVBox: VBox = new VBox {
    this.spacing = 15
    this.alignment = Pos.Center
    this.children = titlePane +: mainMenuButtons
  }

  private val sceneStackPane: StackPane = new StackPane {
    this.styleClass = List("menu-scene")
    this.minHeight = sceneHeight
    this.minWidth = sceneWidth
    this.children = buttonsVBox
  }

  stylesheets = List(getClass.getClassLoader.getResource("styles.css").toExternalForm)
  content = sceneStackPane

  private def stringToDifficulty(string: String): Difficulty.Value = {
    string.toLowerCase match {
      case "easy" => Difficulty.EASY
      case "medium" => Difficulty.MEDIUM
      case "hard" => Difficulty.HARD
      case _ => Difficulty.EASY
    }
  }

  private def stringToBoardSize(string: String): BoardSize.Value = {
    string.toLowerCase match {
      case "8x8" => BoardSize.SMALL
      case "10x10" => BoardSize.MEDIUM
      case "12x12" => BoardSize.LARGE
      case _ => BoardSize.SMALL
    }
  }
}
