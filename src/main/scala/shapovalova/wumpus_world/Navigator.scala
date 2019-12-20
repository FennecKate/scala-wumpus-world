package shapovalova.wumpus_world

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import Navigator.{Req, Breeze, Bump, Glitter, Scream, Stench}
import org.slf4j.LoggerFactory
import shapovalova.wumpus_world.Env.Perception
import shapovalova.wumpus_world.Spelunker.SpelunkerAction

class Navigator {
  private val logger = LoggerFactory.getLogger(Navigator.getClass)

  private def parseMessage(messageText: String): Perception = {
    val sentences = messageText.split("\\. ")

    val percepts = sentences.map(parseSentence)

    var stench: Boolean = false
    var glitter: Boolean = false
    var breeze: Boolean = false
    var scream: Boolean = false
    var bump: Boolean = false

    if (percepts.contains(Stench)) stench = true
    if (percepts.contains(Glitter)) glitter = true
    if (percepts.contains(Breeze)) breeze = true
    if (percepts.contains(Scream)) scream = true
    if (percepts.contains(Bump)) bump = true

    Perception(breeze, bump, glitter, stench, scream)
  }

  private def parseSentence(str: String): Navigator.Percept = {
    if (str.contains("breeze")) Breeze
    else if (str.contains("stench")) Stench
    else if (str.contains("glitter")) Glitter
    else if (str.contains("scream")) Scream
    else if (str.contains("bump")) Bump
    else throw new RuntimeException("Perception string must be about the state of the cave!")
  }

  def navigatorActor: Behavior[Req] = Behaviors.receive((context, message) => {

    val percept = message.perception

    val action = Solver.calculateAction(percept)

    message.sender ! Navigator.Res(action)
    Behaviors.same
  })
}


object Navigator {
  case class Req(perception: Perception, message: String, sender: ActorRef[Res])
  case class Res(action: SpelunkerAction)

  sealed trait Percept

  case object Stench extends Percept
  case object Breeze extends Percept
  case object Scream extends Percept
  case object Glitter extends Percept
  case object Bump extends Percept
}