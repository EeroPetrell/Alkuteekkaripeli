package alkuteekkari

import scala.io.StdIn._

object AlkuteekkariUI extends App {

  private val game = new Adventure
  private val player = game.player
  run


  private def run = {
    while (!game.isOver) {
      printAreaInfo
      game.player.location.name match {
        case "Sokkelo"    => game.objects.playSokkelo //tässä näkyy ainakin minulla virheilmoitus, oma tietämykseni ei riitä sanomaan mistä se tulee, ei vaikuta pelin toimintaan
        case "Arkku"      => game.objects.playArkku
        case other        => playTurn
      }
    }
    println("\n" + this.game.loppuEsittely)
  }
  
  private def printAreaInfo = println("\n" + player.location.fullDescription + "\n")
  
  private def playTurn = {
    player.location.visit
    var command = readLine("")
    var turnReport = this.game.playTurn(command)
    while (turnReport.isDefined) {
      println()
      turnReport.foreach(println)
      println()
      command = readLine("")
      turnReport = this.game.playTurn(command)
    }
  }
}
