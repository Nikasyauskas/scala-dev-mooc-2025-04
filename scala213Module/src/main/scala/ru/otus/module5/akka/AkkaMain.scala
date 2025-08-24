package Akka
import Akka.AkkaMain3.change_behaviour.WorkerProtocol
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, SpawnProtocol}
import akka.util.Timeout

import scala.concurrent.Future
import scala.language.{existentials, postfixOps}
import scala.concurrent.duration._
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.Props

object AkkaMain {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem[String](behaviour_factory_method.Echo(), "Echo")
    system ! "Hello"
    Thread.sleep(1000)
    system.terminate()
  }
}

// root actor
object AkkaMain2 {
  object  Supervisor {
    def apply(): Behavior[SpawnProtocol.Command] = Behaviors.setup{ ctx =>
      ctx.log.info(ctx.self.toString)
      SpawnProtocol()
    }
  }

  def main(args: Array[String]): Unit = {
    implicit  val system = ActorSystem[SpawnProtocol.Command](Supervisor(), "Echo")
    implicit  val ec = system.executionContext
    implicit val timeout = Timeout(3 seconds)

    val echo: Future[ActorRef[String]] = system.ask(
      SpawnProtocol.Spawn(behaviour_factory_method.Echo(), "Echo", Props.empty, _))
    for (ref <- echo)
      ref ! "hello from ask pattern"
  }
}


// change state
object AkkaMain3 {
  object  change_behaviour {
    sealed trait WorkerProtocol
    object WorkerProtocol {
      case object Start extends WorkerProtocol
      case object StandBy extends WorkerProtocol
      case object Stop extends WorkerProtocol
    }

    import WorkerProtocol._
    def apply(): Behavior[WorkerProtocol] = idle()
    def idle(): Behavior[WorkerProtocol] = Behaviors.setup{ctx =>
      Behaviors.receiveMessage{
        case msg@Start =>
          ctx.log.info(msg.toString())
          workInProgress()
        case msg@StandBy =>
          ctx.log.info(msg.toString())
          idle()
        case msg@Stop =>
          ctx.log.info(msg.toString())
          Behaviors.stopped
      }
    }

    def workInProgress(): Behavior[WorkerProtocol] = Behaviors.setup{ctx =>
      Behaviors.receiveMessage{
        case msg@Start => Behaviors.unhandled
        case msg@StandBy =>
          ctx.log.info("go to standby")
          idle()
        case msg@Stop =>
          ctx.log.info("stopped")
          Behaviors.stopped
      }
    }
  }
}

import AkkaMain3.change_behaviour.WorkerProtocol._

object  AkkaMain3App {
  def main(args: Array[String]): Unit = {
    val system: ActorSystem[WorkerProtocol] = ActorSystem(AkkaMain3.change_behaviour(), "Hello")

    system ! Start
    system ! StandBy
    system ! Stop
  }
}

// dispatcher
object task_dispatcher {

  sealed trait  TaskDispatcherProtocol
  case class ParseUrl(url: String) extends TaskDispatcherProtocol
  case class Log(str: String) extends TaskDispatcherProtocol

  case class LogResponseWrapper(msg: LogWorker.ResponseProtocol) extends TaskDispatcherProtocol
  case class ParseResponseWrapper(msg: ParseUrlWorker.ResponseProtocol) extends  TaskDispatcherProtocol

  def apply(): Behavior[TaskDispatcherProtocol] = Behaviors.setup{ctx =>
    val adapter1 = ctx.messageAdapter[LogWorker.ResponseProtocol](rs => LogResponseWrapper(rs))
    val adapter2 = ctx.messageAdapter[ParseUrlWorker.ResponseProtocol](rs => ParseResponseWrapper(rs))

    Behaviors.receiveMessage{
      case ParseUrl(url) =>
        val ref = ctx.spawn(ParseUrlWorker(), s"ParseWorker-${java.util.UUID.randomUUID().toString}")
        ref ! ParseUrlWorker.ParseUrl(url, adapter2)
        Behaviors.same

      case Log(str) =>
        val ref = ctx.spawn(LogWorker(), s"LogWorker-${java.util.UUID.randomUUID().toString}")
        ref ! LogWorker.Log(str, adapter1)
        Behaviors.same

      case LogResponseWrapper(LogWorker.LogDone) =>
        ctx.log.info("LogWorker.LogDone")
        Behaviors.same

      case ParseResponseWrapper(ParseUrlWorker.ParseDone) =>
        ctx.log.info("ParseUrlWorker.ParseDone")
        Behaviors.same
    }
  }



  object LogWorker {
    sealed trait LogProtocol
    case class Log(str: String, replyTo: ActorRef[ResponseProtocol]) extends  LogProtocol

    sealed trait  ResponseProtocol

    case object LogDone extends  ResponseProtocol

    def apply(): Behavior[LogProtocol] = Behaviors.setup{ctx =>
      Behaviors.receiveMessage{
        case Log(str, replyTo) =>
          replyTo ! LogDone
          Behaviors.stopped
      }
    }
  }

  object ParseUrlWorker {
    sealed trait ParseProtocol
    case class ParseUrl(url: String, replyTo: ActorRef[ResponseProtocol]) extends ParseProtocol

    sealed trait  ResponseProtocol
    case object ParseDone extends ResponseProtocol

    def apply(): Behavior[ParseProtocol] = Behaviors.setup{ctx =>
      Behaviors.receiveMessage{
        case ParseUrl(str, replyTo) =>
          replyTo ! ParseDone
          Behaviors.stopped
      }
    }
  }
}


object AkkaMain4App {
  def main(args: Array[String]): Unit = {
    val taskDispatcherRef: ActorSystem[task_dispatcher.TaskDispatcherProtocol] = ActorSystem(task_dispatcher(), "task_dispatcher")
    taskDispatcherRef ! task_dispatcher.Log("this is a log message")
    taskDispatcherRef ! task_dispatcher.ParseUrl("dsafsdf.de")

  }
}



