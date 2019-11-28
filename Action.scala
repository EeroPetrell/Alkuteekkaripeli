package alkuteekkari

class Action(input: String, private val actor: Player) {

  /** Action-luokka päättelee luonnollisella kielellä (suomeksi) annetusta syötteestä tarvittavat komennot ja käskee actor-olioa.
   *  Se jakaa annetun käskyn sanoiksi, joista se annetussa järjestyksessä kokeilee löytää ennalta lueteltuja verbejä. Tällaisen
   *  löydettyään se etsii muista sanoista tarkentavaa substantiivia, mikäli sellainen on tarpeen. Poikkeuksen muodostavat yhden
   *  sanan mittaiset syötteet, jotka käsitellään erillisen protokollan mukaisesti. Mikäli tunnettuja sanoja ei löydy, palautetaan 
   *  virheilmoitus tulostettavaksi.
  */
  
  private val commandWords = input.trim.toLowerCase.split(" ").map( _.trim )
  
  def execute: Option[String] = {
    commandWords.length match {
      case 0     => noWordCase
      case 1     => oneWordCase
      case 2     => twoWordCase
      case 3     => threeWordCase
      case other => Some(Tekstit.liianPitkaKasky)
    }
  }
  
  private def noWordCase = Some(Tekstit.puuttuvaKasky)
  
  private def oneWordCase =  recogniseOneWordOrdersExceptGo
  
  /** Kokeilee etsiä verbiä ensin toisesta sanasta, sen jälkeen ensimmäisestä.
   */
  private def twoWordCase() = {
    val firstOrder = startTrying(1, 0)
    if (firstOrder.isDefined)
      startTrying(0, 1)
    else 
      firstOrder
  }
    
  /** Kokeilee etsiä verbiä ja substantiivia kolmessa eri järjestyksessä.*/
  private def threeWordCase() = {
    val firstOrder = startTrying(0, 1)
    if (firstOrder.isDefined) {
      val secondOrder = startTrying(0, 2)
      if (secondOrder.isDefined) 
        startTrying(1, 2)
      else
        secondOrder
    } else {
      firstOrder
    }
  }
  
  /** Seuraavat kuusi apumetodia kutsuvat itse seuraavaa metodiaan kunnes joko päästään ketjun loppuun (notFound-metodi)
   *  tai löydetään ymmärrettävä tulkinta komennolle. Tällöin käsketään actor-olioa.*/
  private def startTrying(verbIndex: Int, substantIndex: Int): Option[String] = tryGo(verbIndex, substantIndex)
  
  private def tryGo(verbIndex: Int, substantIndex: Int): Option[String] = {
    commandWords(verbIndex) match {
      case "mene"         => recogniseDirectionAndGo(substantIndex)
      case "menen"        => recogniseDirectionAndGo(substantIndex)
      case "liiku"        => recogniseDirectionAndGo(substantIndex)
      case "liikun"       => recogniseDirectionAndGo(substantIndex)
      case "kulje"        => recogniseDirectionAndGo(substantIndex)
      case "kuljen"       => recogniseDirectionAndGo(substantIndex)
      case "kävele"       => recogniseDirectionAndGo(substantIndex)
      case "kävelen"      => recogniseDirectionAndGo(substantIndex)
      case "jatka"        => recogniseDirectionAndGo(substantIndex)
      case "jatkan"       => recogniseDirectionAndGo(substantIndex)
      case "lähde"        => recogniseDirectionAndGo(substantIndex)
      case "lähden"       => recogniseDirectionAndGo(substantIndex)
      case "käänny"       => recogniseDirectionAndGo(substantIndex)
      case "käännyn"      => recogniseDirectionAndGo(substantIndex)
      case "ui"           => recogniseDirectionAndGo(substantIndex)
      case "uin"          => recogniseDirectionAndGo(substantIndex)
      case other          => tryQuit(verbIndex, substantIndex)
    }
  }
  
  private def tryQuit(verbIndex: Int, substantIndex: Int): Option[String] = {
    commandWords(verbIndex) match {
      case "lopeta"       => actor.quit
      case "lopetan"      => actor.quit
      case "luovuta"      => actor.quit
      case "luovutan"     => actor.quit
      case "stop"         => actor.quit
      case other          => tryStudy(verbIndex, substantIndex)
    }
  }
  
  private def tryStudy(verbIndex: Int, substantIndex: Int): Option[String] = {
    commandWords(verbIndex) match {
      case "tutki"        => recogniseDirectionAndGo(substantIndex)
      case "tutkin"       => recogniseDirectionAndGo(substantIndex)
      case "tutkaile"     => recogniseDirectionAndGo(substantIndex)
      case "tutkailen"    => recogniseDirectionAndGo(substantIndex)
      case "katso"        => recogniseDirectionAndGo(substantIndex)
      case "katson"       => recogniseDirectionAndGo(substantIndex)
      case "avaa"         => recogniseDirectionAndGo(substantIndex)
      case "avaan"        => recogniseDirectionAndGo(substantIndex)
      case "kokeile"      => recogniseDirectionAndGo(substantIndex)
      case "kokeilen"     => recogniseDirectionAndGo(substantIndex)
      case "yritä"        => recogniseDirectionAndGo(substantIndex)
      case "yritän"       => recogniseDirectionAndGo(substantIndex)
      case "lue"          => recogniseDirectionAndGo(substantIndex)
      case "luen"         => recogniseDirectionAndGo(substantIndex)
      case "lukea"        => recogniseDirectionAndGo(substantIndex)
      case other          => tryHelp(verbIndex, substantIndex)
    }
  }
  
  private def tryHelp(verbIndex: Int, substantIndex: Int): Option[String] = {
    commandWords(verbIndex) match {
      case "apua"         => actor.help
      case "help"         => actor.help
      case "auta"         => actor.help
      case "ohje"         => actor.help
      case "ohjeet"       => actor.help
      case "ohjeistus"    => actor.help
      case "käyttöohje"   => actor.help
      case "komennot"     => actor.help
      case other          => notFound(verbIndex, substantIndex)
    }
  }
  
  private def notFound(verbIndex: Int, substantIndex: Int): Option[String] = Some(Tekstit.epaselvaKasky)
  
  /** RecogniseOneWordOrdersExceptGo on poikkeusmenettelymetodi yhden sanan mittaisille komennoille.
   *  Tänne päädytään suoraan yhden sanan mittaisissa komennoissa.*/
  private def recogniseOneWordOrdersExceptGo: Option[String] = {
    commandWords(0) match {
      case "lopeta"       => actor.quit
      case "lopetan"      => actor.quit
      case "luovuta"      => actor.quit
      case "luovutan"     => actor.quit
      case "stop"         => actor.quit
      case "seis"         => actor.quit
      case "apua"         => actor.help
      case "help"         => actor.help
      case "auta"         => actor.help
      case "ohje"         => actor.help
      case "ohjeet"       => actor.help
      case "ohjeistus"    => actor.help
      case "käyttöohje"   => actor.help
      case "komennot"     => actor.help 
      case "käänny"       => actor.go("taakse")
      case "käännyn"      => actor.go("taakse")
      case other          => recogniseDirectionAndGo(0)
    }
  }
  
  private def recogniseDirectionAndGo(index: Int): Option[String] = {
    commandWords(index) match {
      case "vasen"        => actor.go("vasemmalle")
      case "vasemmalle"   => actor.go("vasemmalle")
      case "vasempaan"    => actor.go("vasemmalle")
      case "oikea"        => actor.go("oikealle")
      case "oikealle"     => actor.go("oikealle")
      case "oikeaan"      => actor.go("oikealle")
      case "eteen"        => actor.go("eteen")
      case "eteenpäin"    => actor.go("eteen")
      case "suoraan"      => actor.go("eteen")
      case "taakse"       => actor.go("taakse")
      case "takaisin"     => actor.go("taakse")
      case "takasin"      => actor.go("taakse")
      case "palaa"        => actor.go("taakse")
      case "palaan"       => actor.go("taakse")
      case "arkku"        => actor.go("eteen")
      case "arkkua"       => actor.go("eteen")
      case "arkun"        => actor.go("eteen")
      case "avata"        => actor.go("eteen") // kokeilen avata (arkkua)
      case "kirjeen"      => actor.openLetter
      case "kirjettä"     => actor.openLetter
      case "kirje"        => actor.openLetter
      case other          => Some(Tekstit.epaselvaKasky)
    }
  }
}

