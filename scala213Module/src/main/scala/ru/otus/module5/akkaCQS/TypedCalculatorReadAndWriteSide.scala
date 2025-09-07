import akka.NotUsed
import akka.actor.typed.{ActorSystem, Behavior, Props}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.persistence.cassandra.query.scaladsl.CassandraReadJournal
import akka.persistence.query.{EventEnvelope, PersistenceQuery}
import akka.persistence.typed.PersistenceId
import akka.persistence.typed.scaladsl.{Effect, EventSourcedBehavior}
import akka.stream.scaladsl.{Flow, GraphDSL, RunnableGraph, Sink, Source}
import akka.stream.{ClosedShape, Materializer}
import akka_typed.CalculatorRepository.{getLatestOffsetAndResult, initDatabase, updatedResultAndOffset}
import akka_typed.TypedCalculatorWriteSide.{Add, Added, Command, Divide, Divided, Multiplied, Multiply}
import com.typesafe.config.ConfigFactory
import scalikejdbc.{ConnectionPool, ConnectionPoolSettings, DB, using}
import slick.jdbc.PostgresProfile.api._
import com.lightbend.akka.stream.alpakka.slick.scaladsl.{Slick, SlickSession}
import scala.concurrent.duration._
import scala.concurrent.Await


object  akka_typed{

  val persId = PersistenceId.ofUniqueId("001")

  trait CborSerialization

  object TypedCalculatorWriteSide{
    sealed trait Command
    case class Add(amount: Double) extends Command
    case class Multiply(amount: Double) extends Command
    case class Divide(amount: Double) extends Command

    sealed trait Event extends CborSerialization
    case class Added(id:Int, amount: Double) extends Event
    case class Multiplied(id:Int, amount: Double) extends Event
    case class Divided(id:Int, amount: Double) extends Event

    final case class State(value:Double) extends CborSerialization
    {
      def add(amount: Double): State = copy(value = value + amount)
      def multiply(amount: Double): State = copy(value = value * amount)
      def divide(amount: Double): State = copy(value = value / amount)
    }

    object State{
      val empty = State(0.0)
    }


    def handleCommand(
                       persistenceId: String,
                       state: State,
                       command: Command,
                       ctx: ActorContext[Command]
                     ): Effect[Event, State] =
      command match {
        case Add(amount) =>
          ctx.log.info(s"receive adding  for number: $amount and state is ${state.value}")
          val added = Added(persistenceId.toInt, amount)
          Effect
            .persist(added)
            .thenRun{
              x=> ctx.log.info(s"The state result is ${x.value}")
            }
        case Multiply(amount) =>
          ctx.log.info(s"receive multiplying  for number: $amount and state is ${state.value}")
          val multiplied = Multiplied(persistenceId.toInt, amount)
          Effect
            .persist(multiplied)
            .thenRun{
              x=> ctx.log.info(s"The state result is ${x.value}")
            }
        case Divide(amount) =>
          ctx.log.info(s"receive dividing  for number: $amount and state is ${state.value}")
          val divided = Divided(persistenceId.toInt, amount)
          Effect
            .persist(divided)
            .thenRun{
              x=> ctx.log.info(s"The state result is ${x.value}")
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
      Behaviors.setup{ ctx =>
        EventSourcedBehavior[Command, Event, State](
          persistenceId = persId,
          State.empty,
          (state, command) => handleCommand("001", state, command, ctx),
          (state, event) => handleEvent(state, event, ctx)
        )
      }

  }


  case class Result(state: Double, offset: Long)

  case class TypedCalculatorReadSide(system: ActorSystem[NotUsed]) {
    initDatabase

    implicit  val materialiyer = system.classicSystem

    var (offset, latestCalculatedResult) = getLatestOffsetAndResult
    val startOffset = if (offset == 1) 1 else offset + 1

    val readJournal = PersistenceQuery(system).readJournalFor[CassandraReadJournal](
      CassandraReadJournal.Identifier)

    def updateState(event: Any, seqNum: Long): Result = {
      val newState = event match {
        case Added(_, amount) =>
          latestCalculatedResult + amount
        case Multiplied(_, amount) =>
          latestCalculatedResult * amount
        case Divided(_, amount) =>
          latestCalculatedResult / amount
      }
      latestCalculatedResult = newState
      Result(newState, seqNum)
    }




    val source: Source[EventEnvelope, NotUsed] =
      readJournal.eventsByPersistenceId("001", startOffset, Long.MaxValue)

    // Create Slick session
    implicit val slickSession: SlickSession = SlickSession.forConfig("slick-postgres")
    
    val graph = GraphDSL.create() {
      implicit builder: GraphDSL.Builder[NotUsed] =>
        import GraphDSL.Implicits._
        
        // 1. Source
        val input = builder.add(source)
        
        // 2. State updater flow
        val stateUpdater = builder.add(Flow[EventEnvelope].map(e => updateState(e.event, e.sequenceNr)))
        
        // 3. Local save output (for logging/state tracking)
        val localSaveOutput = builder.add(Sink.foreach[Result] { r =>
          latestCalculatedResult = r.state
          println(s"Local state updated: ${r.state}, offset: ${r.offset}")
        })
        
        // 4. Database save output using Slick
        val dbSaveOutput = builder.add(
          Slick.sink[Result] { r =>
            sqlu"UPDATE public.result SET calculated_value = ${r.state}, write_side_offset = ${r.offset} WHERE id = 1"
          }
        )
        
        // 5. Broadcast to split the flow into two outputs
        val broadcast = builder.add(akka.stream.scaladsl.Broadcast[Result](2))
        
        // 6. Connect the graph
        input ~> stateUpdater ~> broadcast ~> localSaveOutput
                              broadcast ~> dbSaveOutput
        
        ClosedShape
    }
    
    // Run the graph
    RunnableGraph.fromGraph(graph).run()


  }

  object CalculatorRepository {
    def createSession(): SlickSession = {
      SlickSession.forConfig("slick-postgres")
    }
    
    def initDatabase(): Unit = {
      val poolSettings = ConnectionPoolSettings(initialSize = 10, maxSize = 100)
      ConnectionPool.singleton("jdbc:postgresql://localhost:5432/demo", "docker", "docker", poolSettings)
    }

    def getLatestOffsetAndResult: (Long, Double) = {
      val entities =
        DB readOnly { session =>
          session.list("select * from public.result where id = 1;") {
            row =>
              (
                row.long("write_side_offset"),
                row.double("calculated_value")
              )
          }
        }
      entities.head
    }

    def updatedResultAndOffset(calculated: Double, offset: Long): Unit = {
      using(DB(ConnectionPool.borrow())){
        db =>
          db.autoClose(true)
          db.localTx{
            _.update("update public.result set calculated_value=?, write_side_offset=? where id=1",
              calculated, offset)
          }
      }
    }
  }

  def apply(): Behavior[NotUsed] =
    Behaviors.setup{
      ctx =>
        val writeAcorRef = ctx.spawn(TypedCalculatorWriteSide(), "Calc", Props.empty)
        writeAcorRef ! Add(10.0)
        writeAcorRef ! Multiply(2.0)
        writeAcorRef ! Divide(5.0)

        Behaviors.same
    }

  def main(args: Array[String]): Unit = {
    val value = akka_typed()
    val config = ConfigFactory.load("application.conf")

    implicit val system: ActorSystem[NotUsed] = ActorSystem(value, "akka_typed", config)
    TypedCalculatorReadSide(system)
    implicit  val actorContext = system.executionContext

  }

}