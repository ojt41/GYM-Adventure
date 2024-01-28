

class Game:



  val lockerRoom = Area("locker room", "You are in the locker rooms hanging out, getting ready or taking a break.")
  val bench = Area("bench", "A bench complete with a safety rack.\nPerfect for bench press.")
  val squatRack = Area("squatrack", "A standard power rack with a barbell.\nPerfect for squats")
  val deadliftPlatform = Area("deadliftplatform", "A deadlift platform.\n Perfect for deadlifts")


  val steroidDealer = new NPC("Steroid Dealer", "A shady character offering performance-enhancing substances.", hasSubstance = true)
  lockerRoom.addNpc(steroidDealer)

  var player: Player = Player(0, lockerRoom)

  lockerRoom.setNear("bench", bench)
  lockerRoom.setNear("squatRack", squatRack)
  lockerRoom.setNear("deadliftPlatform", deadliftPlatform)

  bench.setNear("lockerRoom", lockerRoom)
  squatRack.setNear("lockerRoom", lockerRoom)
  deadliftPlatform.setNear("lockerRoom", lockerRoom)

  def createPlayer(weight: Int, area: Area) =
    this.player = Player(weight, area)

  def isWon = this.player.benchProgress.head >= 100 && this.player.dlProgress.head >= 100 && this.player.squatProgress.head >= 100

  def lostduetoliver = this.player.benchProgress.last >= 30 || this.player.dlProgress.last >= 30 || this.player.squatProgress.last >= 30 || this.player.steroidsUsed > 2

  var turnCount = 0
  var afterSteroids = this.player.roidTurns

  def isComplete = this.player.isLifted && !isLost

  def isLost = this.player.checkInjury || this.player.pop


    
  def welcomeMessage = "You are a tired and depressed university student, who started recently going to the gym." +
    "\n" +
    "\nYou are feeling true joy for the first time since your fuksi year and want to take this new hobby to the limits." +
    "\n" +
    "\nHere starts your journey to the very top of the powerlifting world!" +
    "\n" +
    "\nThe objective of the game is to get to 100 kg max on all 3 exercises. You start with 70kg on all. " +
    "\n" +
    "\nEverytime you train a muscle group, you get 10kg addition to your max but also get likelier to injure yourself. Stretching results in a lower chance of injury." +
    "\nYou can take steroids which increase your maximum lift across all exercices, but Beware! Too many steroids can you end you in a hospital..." +
    "\n" +
    "\nType 'help' at any point to see the list of commands." +
    "\n \n \n"

    
  def goodbyeMessage =
    if this.isWon && !this.lostduetoliver then
      "Finally, at the top of the world. Every limit has been breached, every record broken.\nYou retire peacefully, cementing your legacy as the best to ever do it.\nCongratulations! You won!"
    else if lostduetoliver then
      "You used too many steroids and are in the emergency ward for liver injury. \nYou lost. Try with less steroids next time and do more stretches."
    else
      "You injured yourself and ended up in the hospital. " +
        "Try more stretches next time"
    

  def playTurn(command: String) =
    val action = Action(command)
    val outcomeReport = action.execute(this.player, this.player.location, this.player.location.npcPresent.toMap)
    outcomeReport.foreach(report => println(report))
    if (this.isComplete) then {
      println("\n" + this.goodbyeMessage)
    } else if (this.isLost) then{
      println("\n" + this.goodbyeMessage)
    } else {
      this.turnCount += 1
      this.afterSteroids += 1
    }
    