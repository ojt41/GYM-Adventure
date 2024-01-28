class BuySteroids(actor: Player) extends Action("buy steroids") {
  def execute(): Option[String] = {
    actor.inventory += Item("Steroid", "sussy liquid with injection")
    Some(s"You bought steroids from a sketchy man. Be careful with those! They are now in your inventory. Type 'use' to use the steroids")
  }
}
