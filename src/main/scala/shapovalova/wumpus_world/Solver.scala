package shapovalova.wumpus_world

import org.slf4j.LoggerFactory
import shapovalova.wumpus_world.Env.Perception

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Solver {

  private val logger = LoggerFactory.getLogger(Solver.getClass)

  def calculateAction(percept: Perception): Spelunker.SpelunkerAction = solver.get_actions(percept)._2

  val solver = new Solver

  val START = "start"
  val WAMPUS = "wampus"
  val PIT = "pit"
  val BREEZE = "breeze"
  val STENCH = "stench"
  val SCREAM = "scream"
  val GOLD = "gold"
  val BUMP = "bump"
  var ROOM_STATUS_TRUE = 1
  var ROOM_STATUS_FALSE = 2
  var ROOM_STATUS_POSSIBLE = 3
  var ROOM_STATUS_NO_GOLD_WAY = 4
  var ROOM_STATUS_NO_STATUS: Int = -1
}

class Solver {
  private var agentPosition = Position(0, 0)
  private var agentsWayStory = mutable.ListBuffer[(Int, Int)]()
  private var moveRoom = false
  private var agentX = 0
  private var agentY = 0
  var world: WampusWorldProjection = new WampusWorldProjection

  private def get_actions(wumpusPercept: Perception) = {
    var actions: (Spelunker.Look, Spelunker.SpelunkerAction) = null
    var checking_room = world.worldGrid(agentPosition)
    if (checking_room == null) {
      checking_room = new RoomProjection
      world.worldGrid = world.worldGrid + (agentPosition -> checking_room)
    }
    val room_info = Array[String]()
    if (room_info.contains(Solver.BUMP)) {
      val agentStory = agentsWayStory
      agentStory.addOne((agentPosition.x, agentPosition.y))
      agentPosition.x = agentX
      agentPosition.y = agentY
      if (world.worldGrid(agentPosition).exist != Solver.ROOM_STATUS_TRUE) {
        world.worldGrid(agentPosition).exist = Solver.ROOM_STATUS_TRUE
      }
      moveRoom = false
    }
    else {
      val helpPosition = Position(agentX, agentY)
      world.worldGrid(helpPosition).exist = Solver.ROOM_STATUS_FALSE
    }
    checking_room = world.worldGrid(agentPosition)
    if (checking_room == null) {
      checking_room = new RoomProjection
      world.worldGrid = world.worldGrid + (agentPosition -> checking_room)
    }
    if (checking_room.ok != Solver.ROOM_STATUS_TRUE) checking_room.ok = Solver.ROOM_STATUS_TRUE
    for (event <- room_info) {
      checking_room.addEvent(event)
    }
    updateNeighbors(agentPosition)
    if (world.isWampusAlive && world.wampusRoomCount > 2) {
      val wampusPosition = world.getWampusCoords
      actions = getNextRoomAction(agentPosition, wampusPosition, Spelunker.Shoot)
    }
    else {
      val nextOkRooms = getOkNeighbors(agentPosition)
      var best_candidate = -1
      var candidate_status = -1
      for (i <- nextOkRooms.indices) {
        val candidate_room = nextOkRooms(i)
        if (candidate_room.x > agentPosition.x) {
          best_candidate = i
        }
        else if (candidate_room.y > agentPosition.y) if (candidate_status < 3) {
          candidate_status = 3
        }
        else if (candidate_room.x < agentPosition.x) if (candidate_status < 2) {
          candidate_status = 2
        }
        else if (candidate_status < 1) {
          candidate_status = 1
        }
        best_candidate = i
      }
      actions = getNextRoomAction(agentPosition, nextOkRooms(best_candidate), Spelunker.Forward)
    }

    actions
  }

  private def getNextRoomAction(agentPosition: Position, nextOkRoom: Position, action: Spelunker.SpelunkerAction): (Spelunker.Look, Spelunker.SpelunkerAction) = {
    agentX = agentPosition.x
    agentY = agentPosition.y
    var look: Spelunker.Look = Spelunker.LookUp
    if (agentPosition.y < nextOkRoom.y) {
      agentY += 1
      look = Spelunker.LookUp
    }
    else if (agentPosition.y > nextOkRoom.y) {
      agentY -= 1
      look = Spelunker.LookDown
    }
    else if (agentPosition.x < nextOkRoom.x) {
      agentX += 1
      look = Spelunker.LookRight
    }
    else {
      agentX -= 1
      look = Spelunker.LookLeft
    }
    moveRoom = true
    (look, action)
  }

  private def getOkNeighbors(agentPosition: Position): List[Position] = {
    val okNeighbors = getNeighborsPosition(agentPosition)
    val okPositions = ListBuffer[Position]()
    for (position <- okNeighbors) {
      this.world.worldGrid = this.world.worldGrid + (position -> new RoomProjection)
      if ((this.world.worldGrid(position).ok == Solver.ROOM_STATUS_TRUE
        && this.world.worldGrid(position).noWay != Solver.ROOM_STATUS_TRUE
        && this.world.worldGrid(position).exist != Solver.ROOM_STATUS_FALSE)
        || this.world.worldGrid(position).ok == Solver.ROOM_STATUS_NO_STATUS) okPositions += position
    }
    if (okPositions.isEmpty) {
      val (x: Int, y: Int) = agentsWayStory.last
      okPositions.addOne(Position(x, y))
      this.world.worldGrid(agentPosition).noWay = Solver.ROOM_STATUS_TRUE
    }
    okPositions.toList
  }

  private def getNeighborsImaginaryRoom(agentPosition: Position) = {
    val rightNeighbor = Position(agentPosition.x + 1, agentPosition.y)
    val upNeighbor = Position(agentPosition.x, agentPosition.y + 1)
    val leftNeighbor = Position(agentPosition.x - 1, agentPosition.y)
    val bottomNeighbor = Position(agentPosition.x, agentPosition.y - 1)
    var rightRoom = world.worldGrid(rightNeighbor)
    if (rightRoom == null) {
      rightRoom = new RoomProjection
      world.worldGrid = world.worldGrid + (rightNeighbor -> rightRoom)
    }
    var upRoom = world.worldGrid(upNeighbor)
    if (upRoom == null) {
      upRoom = new RoomProjection
      world.worldGrid = world.worldGrid + (rightNeighbor -> upRoom)
    }
    var leftRoom = world.worldGrid(leftNeighbor)
    if (leftRoom == null) {
      leftRoom = new RoomProjection
      world.worldGrid = world.worldGrid + (rightNeighbor -> leftRoom)
    }
    var bottomRoom = world.worldGrid(bottomNeighbor)
    if (bottomRoom == null) {
      bottomRoom = new RoomProjection
      world.worldGrid = world.worldGrid + (rightNeighbor -> bottomRoom)
    }
    val rooms = Array[RoomProjection](rightRoom, upRoom, leftRoom, bottomRoom)
    rooms
  }

  private def getNeighborsPosition(agentPosition: Position) = {
    val rightNeighbor = Position(agentPosition.x + 1, agentPosition.y)
    val upNeighbor = Position(agentPosition.x, agentPosition.y + 1)
    val leftNeighbor = Position(agentPosition.x - 1, agentPosition.y)
    val bottomNeighbor = Position(agentPosition.x, agentPosition.y - 1)

    Array[Position](rightNeighbor, upNeighbor, leftNeighbor, bottomNeighbor)
  }

  private def updateNeighbors(agentPosition: Position): Unit = {
    val currentRoom = world.worldGrid(agentPosition)

    val roomList = getNeighborsImaginaryRoom(agentPosition)
    if (currentRoom.stench == Solver.ROOM_STATUS_TRUE) {
      world.wampusRoomCount = world.wampusRoomCount + 1
      for (room <- roomList) {
        if (room.wampus == Solver.ROOM_STATUS_NO_STATUS) {
          room.ok = Solver.ROOM_STATUS_POSSIBLE
          room.wampus = Solver.ROOM_STATUS_POSSIBLE
        }
      }
    }
    if (currentRoom.breeze == Solver.ROOM_STATUS_TRUE) for (room <- roomList) {
      if (room.pit == Solver.ROOM_STATUS_NO_STATUS) {
        room.ok = Solver.ROOM_STATUS_POSSIBLE
        room.pit = Solver.ROOM_STATUS_POSSIBLE
      }
    }
    if (currentRoom.breeze == Solver.ROOM_STATUS_FALSE && currentRoom.stench == Solver.ROOM_STATUS_FALSE) for (room <- roomList) {
      room.ok = Solver.ROOM_STATUS_TRUE
      room.wampus = Solver.ROOM_STATUS_FALSE
      room.pit = Solver.ROOM_STATUS_FALSE
    }
  }
}

class WampusWorldProjection() {
  var worldGrid: Map[Position, RoomProjection] = Map[Position, RoomProjection]()
  var isWampusAlive = true
  var wampusRoomCount = 0
  var wampusCoords: Position = _

  def getWampusCoords: Position = {
    var xWampusCoord = 0
    var yWampusCoord = 0
    val keys = worldGrid.keySet

    for (roomPosition <- keys) {
      val room: Option[RoomProjection] = worldGrid.get(roomPosition)
      if (room.nonEmpty && room.get.wampus == Solver.ROOM_STATUS_POSSIBLE) {
        xWampusCoord += roomPosition.x
        yWampusCoord += roomPosition.y
      }
    }
    xWampusCoord /= wampusRoomCount
    yWampusCoord /= wampusRoomCount
    this.wampusCoords = Position(xWampusCoord, yWampusCoord)

    this.wampusCoords
  }
}

class RoomProjection() {
  var exist: Int = Solver.ROOM_STATUS_NO_STATUS
  var stench: Int = Solver.ROOM_STATUS_NO_STATUS
  var breeze: Int = Solver.ROOM_STATUS_NO_STATUS
  var pit: Int = Solver.ROOM_STATUS_NO_STATUS
  var wampus: Int = Solver.ROOM_STATUS_NO_STATUS
  var ok: Int = Solver.ROOM_STATUS_NO_STATUS
  var gold: Int = Solver.ROOM_STATUS_NO_STATUS
  var noWay: Int = Solver.ROOM_STATUS_NO_STATUS

  def addEvent(event_name: String): Unit = {
    event_name match {
      case Solver.START =>

      case Solver.WAMPUS =>
        this.wampus = Solver.ROOM_STATUS_TRUE

      case Solver.PIT =>
        this.pit = Solver.ROOM_STATUS_TRUE

      case Solver.BREEZE =>
        this.breeze = Solver.ROOM_STATUS_TRUE

      case Solver.STENCH =>
        this.stench = Solver.ROOM_STATUS_TRUE

      case Solver.SCREAM =>

      case Solver.GOLD =>
        this.gold = Solver.ROOM_STATUS_TRUE

      case Solver.BUMP =>

    }
  }
}

case class Position(var x: Int, var y: Int)
