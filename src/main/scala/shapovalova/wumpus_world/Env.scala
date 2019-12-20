package shapovalova.wumpus_world

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import Env.{ActionResponse, Continue, EnvironmentResponse, Perception, Request, RoomPosition, Win}
import org.slf4j.LoggerFactory
import shapovalova.wumpus_world.Spelunker.Direction

class Env(layout: String) {

  val envBehavior: Behavior[Request] = Behaviors.receive((context, message) => {

    val killSwitch = message match {
      case Env.EnvironmentRequest(sender) =>
        val environmentState = composeCurrentState()
        sender ! EnvironmentResponse(environmentState)

        false

      case Env.PerformAction(action, sender) =>
        performAction(action)

        val result = if (!agentAlive) Env.Lose else if (isGoldTaken) Win else Continue
        sender ! ActionResponse(result)

        result != Continue
    }

    if (killSwitch) Behaviors.stopped else Behaviors.same
  })

  private val logger = LoggerFactory.getLogger(Env.getClass)

  private val wumpusPositions: RoomPosition = parseWumpusPosition(layout)
  private val goldPosition: RoomPosition = parseGoldPosition(layout)
  private val pitPosition: RoomPosition = parsePitPosition(layout)
  private var isGoldTaken: Boolean = false
  private var isWumpusKilled: Boolean = false
  private var agentHasArrow: Boolean = true
  private val roomSize: (Int, Int) = parseRoomSize(layout)
  private var speleologistPosition: RoomPosition = RoomPosition(0, 0)
  private var spelunkerDirection: Spelunker.Direction = Spelunker.Right
  private var wumpusJustKilled: Boolean = false
  private var speleologistBumped: Boolean = false
  private var agentAlive: Boolean = true

  def getSymbolCoordinates(layout: String, symbol: Char): RoomPosition = {
    val rows = layout.split("\r\n")
    val symbolIndexes = rows.map(_.indexOf(symbol))

    val roomPosition = symbolIndexes.zipWithIndex.maxBy(_._1)
    RoomPosition(roomPosition._1, roomPosition._2)
  }

  def parseWumpusPosition(layout: String): RoomPosition = {
    val symbol = 'W'

    getSymbolCoordinates(layout, symbol)
  }

  def parsePitPosition(layout: String): RoomPosition = {
    val symbol = 'P'

    getSymbolCoordinates(layout, symbol)
  }

  def parseRoomSize(layout: String): (Int, Int) = {
    val rows = layout.split("\r\n")
    val height = rows.length
    val width = rows(0).length
    (height, width)
  }

  def parseGoldPosition(layout: String): RoomPosition = {
    val symbol = 'G'

    getSymbolCoordinates(layout, symbol)
  }

  private def composeCurrentState(): Perception = {
    var stench: Boolean = false
    var glitter: Boolean = false
    var breeze: Boolean = false
    var scream: Boolean = false
    var bump: Boolean = false

    val pos = speleologistPosition

    val adjacentRooms = List(RoomPosition(pos.x - 1, pos.y), RoomPosition(pos.x + 1, pos.y),
      RoomPosition(pos.x, pos.y - 1), RoomPosition(pos.x, pos.y + 1))

    for (r <- adjacentRooms) {
      if (wumpusPositions == r) stench = true
      if (pitPosition == r) breeze = true
    }
    if (pos == goldPosition) glitter = true
    if (wumpusJustKilled) scream = true
    if (speleologistBumped) bump = true

    val result = Perception(breeze, bump, glitter, stench, scream)

    result
  }

  def performAction(speleologistAction: Spelunker.SpelunkerAction): Unit = {

    if (wumpusJustKilled) {
      wumpusJustKilled = false
    }

    speleologistAction match {
      case Spelunker.Grab => tryToGrabGold()
      case Spelunker.Climb => climb()
      case Spelunker.TurnRight => turnRight()
      case Spelunker.TurnLeft => turnLeft()
      case Spelunker.Forward => moveSpeleologistForward()
      case Spelunker.Shoot => tryToKillWumpus()
    }
  }

  def calculateNewPosition(): RoomPosition = {
    val oldPosition = speleologistPosition
    val newPosition = spelunkerDirection match {
      case Spelunker.Down => RoomPosition(speleologistPosition.x, speleologistPosition.y + 1)
      case Spelunker.Left => RoomPosition(speleologistPosition.x - 1, speleologistPosition.y)
      case Spelunker.Right => RoomPosition(speleologistPosition.x + 1, speleologistPosition.y)
      case Spelunker.Up => RoomPosition(speleologistPosition.x, speleologistPosition.y - 1)
    }

    logger.info("Position changed")

    if (newPosition.x >= roomSize._1 || newPosition.x < 0 || newPosition.y < 0 || newPosition.y >= roomSize._2)
      oldPosition else newPosition

  }

  def moveSpeleologistForward(): Unit = {
    val newSpeleologistPosition = calculateNewPosition()
    if (newSpeleologistPosition == speleologistPosition) {
      speleologistBumped = true
    } else {
      speleologistPosition = newSpeleologistPosition
    }
  }

  private def turnRight(): Unit = spelunkerDirection match {
    case Spelunker.Up => spelunkerDirection = Spelunker.Right
    case Spelunker.Down => spelunkerDirection = Spelunker.Left
    case Spelunker.Left => spelunkerDirection = Spelunker.Up
    case Spelunker.Right => spelunkerDirection = Spelunker.Down
  }

  private def turnLeft(): Unit = spelunkerDirection match {
    case Spelunker.Up => spelunkerDirection = Spelunker.Left
    case Spelunker.Down => spelunkerDirection = Spelunker.Right
    case Spelunker.Left => spelunkerDirection = Spelunker.Down
    case Spelunker.Right => spelunkerDirection = Spelunker.Up
  }

  def tryToGrabGold(): Unit = if (speleologistPosition == goldPosition) {
    isGoldTaken = true
  } else {
  }

  def climb(): Unit = {
    this.agentAlive = false
  }

  def tryToKillWumpus(): Unit = if (isAgentFacingWumpus(speleologistPosition, spelunkerDirection) && agentHasArrow) {
    this.wumpusJustKilled = true
    this.isWumpusKilled = true
    this.agentHasArrow = false
  }

  private def isAgentFacingWumpus(position: RoomPosition, direction: Direction): Boolean = {
    val wumpus = this.wumpusPositions
    direction match {
      case Spelunker.Up => position.x == wumpus.x && position.y < wumpus.y
      case Spelunker.Down =>  position.x == wumpus.x && position.y > wumpus.y
      case Spelunker.Right => position.y == wumpus.y && position.x < wumpus.x
      case Spelunker.Left => position.y == wumpus.y && position.x > wumpus.x
    }
  }
}

object Env {
  sealed trait Request
  sealed trait Response

  case class EnvironmentRequest(sender: ActorRef[Response]) extends Request
  case class EnvironmentResponse(percept: Perception) extends Response
  case class PerformAction(action: Spelunker.SpelunkerAction, sender: ActorRef[Response]) extends Request
  case class ActionResponse(actionResult: ActionResult) extends Response


  trait ActionResult

  case object Continue extends ActionResult
  case object Win extends ActionResult
  case object Lose extends ActionResult

  case class RoomPosition(x: Int, y: Int)
  case class Perception(breeze: Boolean, bump: Boolean, glitter: Boolean, stench: Boolean, scream: Boolean)

  def main(args: Array[String]): Unit = {
    val environment = new Env(args(0))
    val navigator = new Navigator
    val spelunker = new Spelunker

    val system: ActorSystem[Any] = ActorSystem(Behaviors.setup[Any] (context => {
      context.log.atInfo().log("Starting wumpus world simulation")
      val envRef = context.spawn(environment.envBehavior, "environment")
      val navRef = context.spawn(navigator.navigatorActor, "snavigator")
      val spelRef = context.spawn(spelunker.setupActor(navRef, envRef), "spelunker")
      Behaviors.same
    }), "system")
  }
}
