package alkuteekkari

import scala.io.StdIn._

class Adventure {

  val title = "Alkuteekkaripeli"

  val koti        = new Area("Koti", Tekstit.koti1, Tekstit.koti2)
  val arkkuhuone  = new Area("Kivikkoinen aukea", Tekstit.arkkuhuone1, Tekstit.arkkuhuone2)
  val kivihuone   = new Area("Hietikko", Tekstit.kivihuone1, Tekstit.kivihuone2)
  val arkku       = new Area("Arkku", Tekstit.arkku1, Tekstit.arkku2)
  val sokkelo     = new Area("Sokkelo", Tekstit.sokkelo1, Tekstit.sokkelo2)
  val sokkeloItse = new Area("SokkeloItse", Tekstit.sokkeloItse1, Tekstit.sokkeloItse2)
  val matalikko   = new Area("Matalikko", Tekstit.matalikko1, Tekstit.matalikko2)

         koti.setNeighbors(Vector("eteen" -> sokkelo, "oikealle" -> arkkuhuone, "vasemmalle" -> kivihuone))
   arkkuhuone.setNeighbors(Vector("taakse" -> koti, "eteen" -> arkku))
    kivihuone.setNeighbors(Vector("taakse" -> koti))
      sokkelo.setNeighbors(Vector("taakse" -> koti)) 

  val player = new Player(koti)
  
  val objects = new Objects(this) //käsittelee pelin "erikoisalueet"
         
  var turnCount = 0
  val timeLimit = 100

  def isComplete = (this.player.location == this.matalikko) 
  def isOver = this.isComplete || this.player.hasQuit || this.turnCount == this.timeLimit
  
  def loppuEsittely = {
    if (this.isComplete)
      Tekstit.voittoTeksti
    else if (this.turnCount == this.timeLimit)
      Tekstit.luovutusTeksti
    else  // game over due to player quitting
      Tekstit.luovutusTeksti
  }
  
  def playTurn(command: String): Option[String] = {
    val action = new Action(command, player)
    val outcomeReport = action.execute
    if (!outcomeReport.isDefined) {
      this.turnCount += 1
      None
    } else 
      outcomeReport
  }
}

