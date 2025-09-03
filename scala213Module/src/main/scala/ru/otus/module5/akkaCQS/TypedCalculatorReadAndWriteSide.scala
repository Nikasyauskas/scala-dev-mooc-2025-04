package ru.otus.module5.akkaCQS

import akka.NotUsed
import akka.actor.typed.{ActorSystem, Behavior, Props}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.persistence.typed.PersistenceId
import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior}
import ru.otus.module5.akkaCQS.akka_typed.TypedCalculatorWriteSide.{Add, Divide, Multiply}


object akka_typed {
  trait CborSerialization
  val persId = PersistenceId.ofUniqueId("001")

  object TypedCalculatorWriteSide {
    sealed trait Command
    case class Add(amount: Int) extends Command
    case class Multiply(amount: Int) extends Command
    case class Divide(amount: Int) extends Command

    sealed trait Event
    case class Added(id: Int, amount: Int) extends Event
    case class Multiplied(id: Int, amount: Int) extends Event
    case class Divided(id: Int, amount: Int) extends Event

    final case class State(value: Int) extends CborSerialization{
      def add(amount: Int): State = copy(value = value + amount)
      def multiply(amount: Int): State = copy(value = value * amount)
      def divide(amount: Int): State = copy(value = value / amount)
    }

    object State {
      val empty = State(0)
    }

    def handleCommand(persistentId: String,
                      state: State,
                      command: Command,
                      ctx: ActorContext[Command]): Effect[Event, State] =
      command match {
        case Add(amount) =>
          ctx.log.info(s"receive adding for number: $amount and state is ${state.value}")
          val added = Added(persistentId.toInt, amount)
          Effect
            .persist(added)
            .thenRun{
              x => ctx.log.info(s"The state result is ${x.value}")
            }
        case Multiply(amount) =>
          ctx.log.info(s"receive multiplying for number: $amount and state is ${state.value}")
          val multiplied = Multiplied(persistentId.toInt, amount)
          Effect
          .persist(multiplied)
          .thenRun{
            x=>ctx.log.info(s"The state result is ${x.value}")
          }
        case Divide(amount) =>
          ctx.log.info(s"receive dividing for number: $amount and state is ${state.value}")
          val divided = Divided(persistentId.toInt, amount)
          Effect
          .persist(divided)
          .thenRun{
            x=>ctx.log.info(s"The state result is ${x.value}")
          }
      }

    def handleEvent(state: State, event: Event, ctx: ActorContext[Command]): State =
      event match {
        case Added(_, amount) =>
          ctx.log.info(s"Handling event Added is: $amount and state is ${state.value}")
          state.add(amount)
        case Multiplied(_, amount) =>
          ctx.log.info(s"Handling event Multiplied is: $amount and state is ${state.value}")
          state.multiply(amount)
        case Divided(_, amount) =>
          ctx.log.info(s"Handling event Divided is: $amount and state is ${state.value}")
          state.divide(amount)
      }

    def apply(): Behavior[Command] =
      Behaviors.setup{ctx =>
        EventSourcedBehavior[Command, Event, State](
          persistenceId = persId,
          State.empty,
          (state, command) => handleCommand("001", state, command, ctx),
          (state, event) => handleEvent(state, event, ctx)
        )
      }

  }

  case class TypedCalculatorReadSide {

  }

  object CalculatorRepository {

  }


  def apply(): Behavior[NotUsed] =
    Behaviors.setup{
      ctx =>
        val writeActorRef = ctx.spawn(TypedCalculatorWriteSide(), "calc", Props.empty)
        writeActorRef ! Add(10)
        writeActorRef ! Multiply(2)
        writeActorRef ! Divide(5)
        Behaviors.same
    }

  def main(args: Array[String]): Unit = {
    val value = akka_typed()
    implicit  val system: ActorSystem[NotUsed] = ActorSystem(value, "akka_typed")

    //todo read side
  }




}