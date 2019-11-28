package alkuteekkari

import scala.collection.mutable.Map

class Area(var name: String, val firstDescription: String, val description: String) {

  private val neighbors = Map[String, Area]()
  //private val items = Map[String, Item]()
  private var beenThere = false
  
  def been = beenThere
  def visit = beenThere = true
  
  //def addItem(item: Item) = items += item.name -> item
  
  /*def removeItem(itemName: String): Option[Item] = {
    if (contains(itemName)) {
      val returnItem = items.get(itemName)
      items -= itemName
      returnItem
    } else 
      None
  }*/
  
 // def contains(itemName: String) = items.contains(itemName)

  def neighbor(direction: String) = this.neighbors.get(direction)
  
  def setNeighbor(direction: String, neighbor: Area) = {
    this.neighbors += direction -> neighbor
  }

  def setNeighbors(exits: Vector[(String, Area)]) = {
    this.neighbors ++= exits
  }
  
  def fullDescription = {
    if (beenThere) description else firstDescription
  }

  override def toString = this.name + ": " + this.description.replaceAll("\n", " ").take(150)

}
