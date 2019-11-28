package alkuteekkari

import scala.io.StdIn._

class Objects(game: Adventure) {
  
  /** Objects-luokasta luodaan aina yksi ilmentymä Adventure-luokan pelioliolle tämän luonnin yhteydessä.
   *  Luokan tehtävä on käsitellä kaksi pelistä löytyvää erikoisaluetta: arkku ja sokkelo. Sen ainoat 
   *  julkiset metodit ovat playArkku ja playSokkelo, joita kutsutaan pelaajan mennessä näille alueille.
   *  Muut metodit ovat näiden kahden apumetodeita.
   */

  def playArkku = {
    if (game.arkku.been) {
      tutkiArkkuaLisaa
    } else {
      game.arkku.visit
      var tutkitaanko = 0
      do {
        tutkitaanko = yesOrNo(readLine(""))
      } while (tutkitaanko == 0)
      if (tutkitaanko > 0) tutkiArkkuaLisaa else alaTutkiArkkuaLisaa
    }
  }
    
  private def yesOrNo(command: String) = {
    val words = command.split(" ").map( _.trim ).map( _.toLowerCase )
    val positive = Vector[String]("kyllä", "joo", "juu", "haluan", "toki", "todellakin", "tietysti", "jatkan", "jatka", "tutkin", "tutki", "yritä", "yritän")
    val negative = Vector[String]("ei", "en", "lopetan")
    if (words.exists( negative.contains(_) ))
      -1 //means no
    else if (words.exists( positive.contains(_) ))
      1 // means yes
    else
      0 // means unclear
  }
  
  private def tutkiArkkuaLisaa = {
    println("\n" + Tekstit.arkkuLisatutkimus + "\n")
    var numbersTried = toInt(readLine(""))
    while (numbersTried == None) numbersTried = toInt(readLine(""))
    if (numbersTried.getOrElse(0)/10 == 63) arkkuCleared else arkkuFailed
  }
  
  private def toInt(s: String): Option[Int] = {
    try {
      Some(s.toInt)
    } catch {
      case e: Exception => None
    }
  }
  
  private def alaTutkiArkkuaLisaa = {
    println("\n" + Tekstit.alaTutkiArkkuaLisaa)
    game.player.goTo(game.koti)
  }
  
  private def arkkuCleared = {
    println("\n" + Tekstit.arkkuAukeaa)
    game.player.getLetter
    game.player.goTo(game.koti)
  }
  
  private def arkkuFailed = {
    println("\n" + Tekstit.arkkuEiAukea)
    game.player.goTo(game.koti)
  }



  def playSokkelo = {
    game.sokkelo.visit
    var tutkitaanko = 0
      do {
        tutkitaanko = yesOrNo(readLine(""))
      } while (tutkitaanko == 0)
      if (tutkitaanko > 0) trySokkelo else dontTrySokkelo
  }
  
  private def trySokkelo = {
    game.player.goTo(game.sokkeloItse)
    if (!game.sokkeloItse.been) println("\n" + Tekstit.sokkeloItse1 + "\n") else println("\n" + Tekstit.sokkeloItse2 + "\n")
    game.sokkeloItse.visit
    val command = readLine("")
    var cleaned = command.toLowerCase.split(", ").map( _.trim )
    if (cleaned.length == 1)
      cleaned = command.toLowerCase.split(" ").map( _.trim )
    if (testSokkelo(cleaned)) sokkeloCleaned else sokkeloFailed
  }
  
  private def dontTrySokkelo = {
    println("\n" + Tekstit.labyrinttiLuovutus)
    game.player.goTo(game.koti)
  }

  private def testSokkelo(cleaned: Array[String]) = {
    var returning = true
    if (cleaned.length != 6) returning = false
    if (cleaned.lift(0) != Some("oikealle")) returning = false
    if (cleaned.lift(1) != Some("suoraan")) returning = false
    if (cleaned.lift(2) != Some("suoraan")) returning = false
    if (cleaned.lift(3) != Some("vasemmalle")) returning = false
    if (cleaned.lift(4) != Some("suoraan")) returning = false
    if (cleaned.lift(5) != Some("suoraan")) returning = false
    returning
  }
  
  private def sokkeloCleaned = {
    println("\n" + Tekstit.labyrinttiVoitto)
    game.player.goTo(game.matalikko)
  }
  
  private def sokkeloFailed = {
    println("\n" + Tekstit.labyrinttiPieleen)
    game.player.goTo(game.koti)
  }
}
