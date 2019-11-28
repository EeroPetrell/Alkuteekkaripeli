package alkuteekkari

import scala.collection.mutable.Map

class Player(startingArea: Area) {

  private var currentLocation = startingArea
  private var quitCommandGiven = false
  private var hasLetter = false
    
  def location = this.currentLocation
  
  def hasQuit = this.quitCommandGiven
  
  def quit = {
    quitCommandGiven = true
    None
  }
  
  def getLetter = hasLetter = true
  
  def openLetter: Option[String] = {
    if (hasLetter)
      Some(Tekstit.kirje)
    else
      Some(Tekstit.kirjePuuttuu)
  }
  
  def help: Option[String] = Some(Tekstit.apua)
  
  def go(direction: String) = {
    val destination = this.location.neighbor(direction)
    this.currentLocation = destination.getOrElse(this.currentLocation)
    if (destination.isDefined) None else Some("Et voi liikkua " + direction + ".")
  }

  /** goTo-metodi on object-olion erikoisalueiden sisäiseen käyttöön tarkoitettu paikasta toiseen "hyppimisen" mahdollistava metodi.
   */
  def goTo(goTo: Area) = currentLocation = goTo
  
  override def toString = "Olet nyt " + this.location.name
}