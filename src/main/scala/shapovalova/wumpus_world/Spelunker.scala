package shapovalova.wumpus_world

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import org.slf4j.LoggerFactory
import shapovalova.wumpus_world.Env.{ActionResult, Continue}

class Spelunker {

  private val logger = LoggerFactory.getLogger(Spelunker.getClass)

  private var navigator: ActorRef[Navigator.Req] = _
  private var environment: ActorRef[Env.Request] = _

  private var speakerToEnv: ActorRef[Env.Response] = _
  private var speakerToNavigator: ActorRef[Navigator.Res] = _

  private var currentState: ActionResult = Continue

  def setupActor(navRef: ActorRef[Navigator.Req], envRef: ActorRef[Env.Request]): Behavior[Any] =
    Behaviors.setup(context => {
      this.navigator = navRef
      this.environment = envRef

      if (speakerToEnv == null) {
        speakerToEnv = context.spawn(environmentBehavior, "speleologist-behavior")
        speakerToNavigator = context.spawn(navigatorBehavior, "speleologist-navigator")
      }

      envRef ! Env.EnvironmentRequest(speakerToEnv)

      Behaviors.stopped
    })

  private def environmentBehavior: Behavior[Env.Response] = Behaviors.receive[Env.Response]((context, message) => {
    message match {
      case Env.EnvironmentResponse(percept) =>
        navigator ! Navigator.Req(percept, "", speakerToNavigator)

        Behaviors.same

      case Env.ActionResponse(actionResult: ActionResult) =>
        this.currentState = actionResult

        if (this.currentState == Env.Win) {
          logger.info("Spelunker has taken the gold")
        }
        Behaviors.same
    }
  })

  private def navigatorBehavior: Behavior[Navigator.Res] = Behaviors.receive[Navigator.Res]((context, message) => {
    environment ! Env.PerformAction(message.action, speakerToEnv)

    Behaviors.same
  })
}

object Spelunker {
  sealed trait SpelunkerAction

  case object Climb extends SpelunkerAction
  case object Forward extends SpelunkerAction
  case object TurnLeft extends SpelunkerAction
  case object TurnRight extends SpelunkerAction
  case object Grab extends SpelunkerAction
  case object Shoot extends SpelunkerAction

  sealed trait Look
  case object LookUp extends Look
  case object LookDown extends Look
  case object LookLeft extends Look
  case object LookRight extends Look

  sealed trait Direction

  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction
}
