class Action(input: String):
  
  val commandText = input.trim.toLowerCase
  val verb = commandText.takeWhile( _ != ' ' )
  val modifiers = commandText.drop(verb.length).trim

  def execute(actor: Player, area: Area, npcs: Map[String, NPC]): Option[Any] = this.verb match {
    case "train" => Some(actor.training(this.modifiers))
    case "stretch" => Some(actor.training("stretch"))
    case "go" => Some(actor.go(this.modifiers, area))
    case "quit" => Some(actor.quit())
    case "use" => Some(actor.use())
    case "inventory" => Some(actor.showInventory)
    case "buy" => (new BuySteroids(actor)).execute()
    case "help" =>
                    Some("Available commands: go, talk, train, stretch, quit, use, accept, inventory, help")
    case "talk" =>
                    println("NPC says: You looking to get Jacked? I can sell you some stuff")
                    Some("Type 'jackmeup' to respond the steroid dealer")
    case "jackmeup" =>
                    Some("NPC says 'I see' and pulls out a sussy vial. Are you sure you want to buy? (Type 'buy' to confirm)")

    case "stats" =>
      Some(actor.showStats())
    case other => Some("Not a valid command. Type 'help' to check list of commands")

  }
    

  override def toString = s"$verb (modifiers: $modifiers)"
  
