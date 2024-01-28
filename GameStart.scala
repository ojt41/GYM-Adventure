import scala.io.StdIn.readLine

object GameStart extends App:

  private val game = Game()
  println(this.game.welcomeMessage)
  val mass = readLine("Please input your player's mass: ").toInt
  this.game.createPlayer(mass, game.lockerRoom)
  private val player = this.game.player

  this.run()

  private def run() =
    println("You start the game with these stats. Train your way up to 100 kg max and accomplish your goal")
    println(this.game.player.showStats())
    while !this.game.isWon && !this.game.lostduetoliver do
      this.printAreaInfo()
      this.playTurn()
    println("\n" + this.game.goodbyeMessage)
  
  
  private def printAreaInfo() =
    val area = this.player.location
    println("\n\n" + area.name)
    println("-" * area.name.length)
    println(area.fullDescription + "\n")


  private def playTurn() =
    println()
    val command = readLine("Command: ")
    val turnReport = this.game.playTurn(command)