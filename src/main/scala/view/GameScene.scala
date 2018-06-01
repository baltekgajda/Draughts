package view

import controller.Controller
import draughtsLogic.Board
import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.scene.Scene
import scalafx.scene.control.Button
import scalafx.scene.input.MouseEvent
import scalafx.scene.layout._
import scalafx.scene.paint.Color
import scalafx.scene.shape.Rectangle

case class GameScene(sceneWidth: Double, boardSize: Int = 8) extends Scene {

  private val tileSize = (sceneWidth - 40) / boardSize

  private val tilesPane: Pane = new Pane() {
    this.children = for (x <- 0 until boardSize; y <- 0 until boardSize) yield GameScene.createTile(x, y, tileSize)
  }

  private val gameBoardStackPane: StackPane = {
    val stackPane = new StackPane
    stackPane.children = List(tilesPane, PiecePane(boardSize, tileSize, Board(boardSize), this))
    stackPane
  }

  private val buttonsHBox: HBox = {
    val hbox = new HBox {
      this.spacing = 20
      val newGameButton: Button = GameScene.createButton(2 * tileSize, tileSize, "New Game")
      val mainMenuButton: Button = GameScene.createButton(2 * tileSize, tileSize, "Main Menu")
      children = List(newGameButton, mainMenuButton)
    }
    hbox.newGameButton.onMouseReleased =
      (e: MouseEvent) => Controller.loadNewGame(this)
    hbox.mainMenuButton.onMouseReleased =
      (e: MouseEvent) => Controller.returnToMainMenu()
    hbox
  }

  private val sceneBorderPane: BorderPane = new BorderPane {
    this.styleClass = List("game-scene")
    this.prefHeight = sceneWidth + 20 + tileSize
    this.prefWidth = sceneWidth
    this.padding = Insets(20, 20, 20, 20)
    this.top = gameBoardStackPane
    this.bottom = buttonsHBox
  }

  stylesheets = List(getClass.getClassLoader.getResource("styles.css").toExternalForm)
  content = sceneBorderPane

  def showEndGameAlert(): Unit = {
    val alert = GameScene.createButton(200, 200, "Game over")
    alert.styleClass = List("end-game-info")
    gameBoardStackPane.children.add(alert)
  }

  def updatePieces(boardMatrix: Board): Unit = {
    gameBoardStackPane.children.clear()
    gameBoardStackPane.children = List(tilesPane, PiecePane(boardSize, tileSize, boardMatrix, this))
  }
}

object GameScene {

  def createTile(_x: Int, _y: Int, size: Double): Rectangle = new Rectangle {
    this.width = size
    this.height = size
    this.x = _x * size
    this.y = _y * size
    this.fill = if ((_x + _y) % 2 == 0) Color.SlateGrey
    else Color.WhiteSmoke
  }

  //TODO dziwne rozmiary przycisku przy zmianie ilośc tiles
  def createButton(bWidth: Double, bHeight: Double, text: String): Button = new Button(text) {
    this.prefHeight = bHeight
    this.prefWidth = bWidth
    this.styleClass = List("tile-button")
  }
}
