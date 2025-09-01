package ru.otus.module5.streaming

import akka.NotUsed
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

sealed trait Command
case class StartChild(name: String) extends Command
case class SendMessageToChild(name: String, msg: String, num: Int) extends Command
case class StopChild(name: String) extends Command
case object Stop extends Command

object Parent {
  def apply(): Behavior[Command] = withChildren(Map())

  def withChildren(children: Map[String, ActorRef[Command]]): Behavior[Command] =
    Behaviors.setup{ctx =>
      Behaviors.receiveMessage{
        case StartChild(name) =>
          ctx.log.info(s"start child $name")
          val newChild = ctx.spawn(Child(), name)
          withChildren(children + (name -> newChild))
        case msg@SendMessageToChild(name, _, i) =>
          ctx.log.info(s"Send message to child $name num=$i")
          val childOption = children.get(name)
          childOption.foreach(childRef => childRef ! msg)
          Behaviors.same
        case StopChild(name) =>
          ctx.log.info(s"Stopping child with name $name")
          val childOption = children.get(name)
          childOption match {
            case Some(childRef) =>
              ctx.stop(childRef)
              Behaviors.same
            case None => Behaviors.same
          }
        case Stop =>
          ctx.log.info("Stop parent")
          Behaviors.stopped
      }
    }

}

object Child {
  def apply(): Behavior[Command] = Behaviors.setup{ctx =>
    Behaviors.receiveMessage{msg =>
      ctx.log.info(s"Child got the message $msg")
      Behaviors.same
    }
  }

}


object StartStopApp extends App {
  def apply(): Behavior[NotUsed] =
    Behaviors.setup{ctx =>
      val parent = ctx.spawn(Parent(), "parent")
      parent ! StartChild("child1")
      parent ! SendMessageToChild("child1", "message to child1", 0)
      parent ! StopChild("child1")
      for (i <- 1 to 15) parent ! SendMessageToChild("child1", "message to child1", i)
      Behaviors.same
    }

  val value = StartStopApp()
  implicit val system = ActorSystem(value, "akka_typed")
  Thread.sleep(5000)
  system.terminate()

}


