package ru.otus.module4.catsstreaming


import cats.effect.kernel.Async
import cats.effect.std.Queue
import cats.effect.{IO, IOApp, Resource}
import fs2.{Chunk, Pure, Stream, io, text}


import java.nio.file.Paths
import java.time.Instant
import scala.concurrent.duration._

object Stream extends IOApp.Simple {
  val pureApply: fs2.Stream[Pure, Int] = fs2.Stream.apply(1,2,3)
  val ioApply: fs2.Stream[IO, Int] = pureApply.covary[IO]
  val list = List(1,2,3,4)
  val strm1: fs2.Stream[Pure, Int] = fs2.Stream.emits(list)

  val a: List[Int] = pureApply.toList
  val aa: IO[List[Int]] = ioApply.compile.toList

  val unfoalded: Stream[IO, String] = fs2.Stream.unfoldEval(0) {s=>
    val next = s + 10
    if (s >= 50) IO.none
    else IO.println(next.toString).as(Some((next.toString, next)))
  }

  val s  = fs2.Stream.eval(IO.readLine).evalMap(s=>IO.println(s">>$s")).repeatN(3)

  type Description = String
  def openFile: IO[Description] = IO.println("open file").as("file desctription")
  def closeFile(desc: Description): IO[Unit] = IO.println("closing file")
  def readFile(desc: Description): fs2.Stream[IO, Byte] = fs2.Stream.emits(s"File content".map(_.toByte).toArray)

  val fileResource: Resource[IO, Description] = Resource.make(openFile)(closeFile)
  val resourceStream: fs2.Stream[IO, Unit] =
    fs2.Stream.resource(fileResource).flatMap(readFile).map(b=>b.toInt+100).evalMap(x=>IO.println(x))

  val infiniteStream: fs2.Stream[Pure, Int] = fs2.Stream.iterate(0)(_+1)


  def writeToSocket[F[_]: Async](chunk: Chunk[String]): F[Unit] =
    Async[F].async_{callback =>
      println(s"zsdfsdf")
      callback(Right())
    }

  fs2.Stream((1 to 100).map(_.toString): _*)
    .chunkN(10)
    .covary[IO]
    .parEvalMapUnordered(10)(writeToSocket[IO])
    .compile
    .drain

  val fixedDelayStream = fs2.Stream.fixedDelay[IO](1.second).evalMap(_ => IO.println(Instant.now()))
  val fixedRateStream = fs2.Stream.fixedRate[IO](1.second).evalMap(_ => IO.println(Instant.now()))

  val queueIO = cats.effect.std.Queue.bounded[IO, Int](100)
  def putInQueue(queue: Queue[IO, Int], value: Int) = queue.offer(value)

  val queueStreamIO: IO[Stream[IO, Int]] = for {
    q <- queueIO
    _ <- (IO.sleep(5.millis) *> putInQueue(q,5)).replicateA(10).start
  } yield fs2.Stream.fromQueueUnterminated(q)

  val queueStream: fs2.Stream[IO, Int] = fs2.Stream.force(queueStreamIO)


  val run: IO[Unit] = resourceStream.compile.drain
}

object Fs2FileChunkExample extends IOApp.Simple {
  val inputFilePath = Paths.get("G://input.txt")
  val outputFilePath = Paths.get("G://output.txt")

  val fileProcessingStream: fs2.Stream[IO, Unit] = {
    io.file.Files[IO]
      .readAll(inputFilePath, 4096)
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .chunkN(2)
      .map(chunk => chunk.map(line=> s"Processed: $line"))
      .flatMap(chunk => fs2.Stream.emits(chunk.toList))
      .intersperse("\n")
      .through(text.utf8.encode)
      .through(io.file.Files[IO].writeAll(outputFilePath))
  }

  val run:IO[Unit] = fileProcessingStream.compile.drain

}