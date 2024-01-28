import scala.collection.mutable

// Player.scala
import scala.collection.mutable.Buffer
class Player(var weight: Int, var location: Area) {
  var inventory = Buffer[Item]()
  var squatProgress = Buffer[Int](70, 0)
  var benchProgress = Buffer[Int](70, 0)
  var dlProgress = Buffer[Int](70, 0)
  var quitCommand = false
  var hasItem = false
  var isInjured = false
  var currentNumbers = Buffer[Double](0, 0, 0)
  var goalsToBeat = Buffer[Double](100, 100, 100) // Sample lifting goals (adjust as needed)
  var turnsAfterRoiding = 99
  var isRoiding = turnsAfterRoiding < 5
  var steroidCheck = false
  var steroidsUsed = 0

  def addToInventory(item: Item): Unit = {
    inventory += item
  }

  def showInventory: String = {
    if (inventory.nonEmpty) then{
      s"Inventory: ${inventory.map(_.name).mkString(", ")}"
    } else {
      "Your inventory is empty."
    }
  }

  def roidTurns: Int = this.turnsAfterRoiding

  def weighins(): Unit = {
    // Set goals based on weight
    // Update goalsToBeat Buffer based on weight categories
    goalsToBeat = Buffer[Double](weight * 0.5, weight * 1.0, weight * 1.5)
  }

  def initNumbers(): Unit = {
    // Initialize currentNumbers based on weight
    currentNumbers = Buffer.fill(3)(0.0)
  }

  def updateNumbers(exercise: String): Unit = {
    // Update currentNumbers based on exercise progress
    val progressBuffer = exercise match {
      case "bench press" => benchProgress
      case "squats" => squatProgress
      case "deadlifts" => dlProgress
      case _ => Buffer(0, 0, 0)
    }

    for (i <- progressBuffer.indices) {
      currentNumbers(i) = progressBuffer(i) * 10.0 // Adjust the factor as needed
    }
  }

  def hasQuit: Boolean = this.quitCommand

    def go(place: String, area: Area): String = {
      val destination = this.location.near(place.toLowerCase)
      this.location = destination.getOrElse(this.location)
      if (destination.isDefined) then{
        s"You go to the $place. \nType 'train' or 'stretch' followed by the exercise"
      } else {
        s"You can't go to the $place."
      }
    }

  def quit(): String = {
    this.quitCommand = true
    "You quit! You gave up on your dreams..."
  }

  def use(): String = {
    if this.inventory.nonEmpty then
      this.steroidsUsed += 1
      benchProgress(0) += 10
      squatProgress(0) += 10
      dlProgress(0) += 10
      benchProgress(1) += 10
      squatProgress(1) += 10
      dlProgress(1) += 10
      "You use steroids. Be careful about the side effects! You are likelier to get injured if you use steroids. Type 'stats' to see current stats of your player. Using steroids increases your maximum lift"
    else
      "Inventory empty"
  }

  def injure(group: String): Unit = {
    // Check for injuries based on exercise progress
    val progressBuffer = group match {
      case "bench" => benchProgress
      case "squat" => squatProgress
      case "dl" => dlProgress
      case _ => Buffer(0, 0)
    }

    // Simulate injury based on progress
    val totalProgress = progressBuffer.sum.toDouble
    val maxProgress = progressBuffer.length * 5.0 // Assuming a maximum safe progress per exercise
    val injuryChance = totalProgress / maxProgress

    if (scala.util.Random.nextDouble() < injuryChance) then{
      isInjured = true
    }
  }

  def training(exercise: String): String = {
    if (this.location.name == "bench") then{
      exercise match {
        case "bench" =>
          benchProgress(0) += 10
          benchProgress(1) += 5
          println("You train until you beat your maximum record. Check stats to find your updated progress")
        case "stretch" =>
          benchProgress(1) -= 10
          println("You stretch and now are less likely to get injured. Check stats to find your updated progress")
        case _ =>
          println("invalid action")
      }
    } else if (this.location.name == "squatrack") then{
      exercise match {
        case "squats" =>
          squatProgress(0) += 10
          squatProgress(1) += 5
          println("You train until you beat your maximum record. Check stats to find your updated progress")
        case "squat" =>
          squatProgress(0) += 10
          squatProgress(1) += 5
          println("You train until you beat your maximum record. Check stats to find your updated progress")
        case "stretch" =>
          squatProgress(1) -= 10
           println("You stretch and now are less likely to get injured. Check stats to find your updated progress")
        case _ =>
          println("invalid action")
      }
    } else if (this.location.name == "deadliftplatform") then{
      exercise match {
        case "deadlifts" =>
          dlProgress(0) += 10
          dlProgress(1) += 5
          println("You train until you beat your maximum record. Check stats to find your updated progress")
        case "deadlift" =>
          dlProgress(0) += 10
          dlProgress(1) += 5
          println("You train until you beat your maximum record. Check stats to find your updated progress")
        case "stretch" =>
          dlProgress(1) -= 1
          println("You stretch and now are less likely to get injured. Check stats to find your updated progress")
        case _ =>
          println("invalid action")
      }
    }

    s"You train $exercise at ${this.location.name}."
  }

  def isLifted: Boolean = {
    // Check if player has achieved lifting goals
    currentNumbers(0) >= goalsToBeat(0) && currentNumbers(1) >= goalsToBeat(1) && currentNumbers(2) >= goalsToBeat(2)
  }



  def checkInjury: Boolean = this.isInjured

  def pop: Boolean = this.isLifted && this.isRoiding

  def notLegit: Boolean = this.steroidCheck

  def inventoryString: String = {
    if (this.hasItem) then{
      "You have some performance-enhancing substances in your pocket."
    } else {
      "You have nothing on you."
    }
  }

  def showStats(): String = {
    s"Current Stats:\n" +
      s"Weight: $weight\n" +
      s"Bench Press Progress: ${benchProgress.head} kg\n" +
      s"Squat Progress: ${squatProgress.head} kg\n" +
      s"Deadlift Progress: ${dlProgress.head} kg\n" +
      s"Injury Proneness: ${calculateInjuryProneness()}"
  }

  private def calculateInjuryProneness(): String = {
    val benchInjury = benchProgress(1)
    val squatInjury = squatProgress(1)
    val dlInjury = dlProgress(1)

    s"Bench Press Injury Proneness: $benchInjury%\n" +
      s"Squat Injury Proneness: $squatInjury%\n" +
      s"Deadlift Injury Proneness: $dlInjury%"
  }


  override def toString: String = {
    val basic =
      s"Now at: ${this.location.name} ${this.dlProgress.mkString(" ")} ${this.benchProgress.mkString(" ")} ${this.squatProgress.mkString(" ")}"
    if (this.turnsAfterRoiding < 5) then{
      basic + "\nYou are feeling ENHANCED"
    } else {
      basic
    }
  }
}
