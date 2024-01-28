import scala.collection.mutable.Map

/** The class "Area" represents different locations in the game word (which is a gym).
  * The game consists of a few places. The locker room, where the player begins their journey
  * and all the gym areas that are for training the 3 main lifts, bench press, squat and deadlift.
  * The player can be located in the areas and can move between areas, if they are near.
  * An area has both a name and a description*/

class Area(var name: String, var description: String):

  var accessible = Map[String, Area]()
  var npcPresent = Map[String, NPC]()

  
/** Returns the areas */
  def hasNpc(npcName: String): Boolean = this.npcPresent.contains(npcName)

  def getNpc(npcName: String): Option[NPC] = this.npcPresent.get(npcName)

  def near(placeName: String) = this.accessible.get(placeName)
  
  def setNear(placeName: String, place: Area) =
    this.accessible += placeName.toLowerCase -> place

  def getNear(placeName: String): Option[Area] =
    this.accessible.get(placeName)

  def fullDescription: String = {
    val places: String = "\n\nAvailable places: " + this.accessible.keys.mkString(", ")
    val npcs: String =
      if (this.npcPresent.nonEmpty) then{
        "\nYou meet: " + this.npcPresent.keys.mkString(", ") + "\nTalk to the NPC by typing 'talk' or use another valid command"

      } else ""

    this.description + npcs + places
  }

  def addNpc(npc: NPC) =
    this.npcPresent += npc.name -> npc

  def npcCheck(npc: NPC) =
    this.npcPresent.contains(npc.name)


end Area