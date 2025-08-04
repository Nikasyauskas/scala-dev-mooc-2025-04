package ru .otus.module3.catsresources

import cats.{Monad, MonadError}
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.effect.{Async, ExitCode, IO, IOApp, Sync}
import cats.syntax.all.catsSyntaxParallelSequence_

import scala.concurrent.duration._
import cats.syntax.all._
import cats.syntax.functor._
import cats.syntax.flatMap._

import scala.io.StdIn
import cats.effect.kernel.Clock

/*
object catsresources1 extends App {
  def divide[F[_]](a: Int, b: Int)(implicit  ME: MonadError[F, String]): F[Int] = {
    if (b == 0) ME.raiseError("divide by zero")
    else (a/b).pure[F]
  }

  import cats.instances.either._
  type EitherString[A] = Either[String, A]
  val result1 = divide[EitherString](10,2) // right 5
  val result2 = divide[EitherString](10,0) // left error

  println(result1)
  println(result2)
}
*/
/*
object catsresource2 extends IOApp {
  def example[F[_]: Async]: F[Unit] = {
    val task = Async[F].sleep(10.seconds) *> Async[F].delay(println("Done"))
    val safeTask = Async[F].uncancelable(_ => task)
    safeTask
  }

  override def run(args: List[String]): IO[ExitCode] =
    example[IO].as(ExitCode.Success)

}
*/
/*
object ClockExample extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    val program = for {
      start <- Clock[IO].monotonic
      _ <- IO.sleep(1.second)
      end <- Clock[IO].monotonic
      delta = end - start
      _ <- IO.println(s"time : $delta")
    } yield ()
    program.as(ExitCode.Success)

  }
}

*/
/*
object ConsoleApp extends IOApp {
  def printLine[F[_]: Sync](line: String): F[Unit] =
    Sync[F].delay(println(line))

  def readLine[F[_]: Sync]: F[String] =
    Sync[F].delay(StdIn.readLine())

  def program[F[_]: Sync]: F[Unit] = {
    val p1 =  printLine[F]("enter the name:")
    val p2 = readLine[F]
    val p3 = printLine[F](s"done")

    p1.flatMap(_ => p2.flatMap(_=>p3))


  }

  override def run(args: List[String]): IO[ExitCode] =
    program[IO].as(ExitCode.Success)
}
*/

import cats.{Monad, MonadError}
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Resource, Sync}
import cats.implicits.catsSyntaxApply
import cats.MonadError
import cats.data.State
import cats.effect.{IO, IOApp}
import cats.implicits._
import cats.effect.unsafe.implicits.global
import cats.effect.kernel._
import java.io.{BufferedReader, FileReader}
import scala.collection.mutable
import scala.concurrent.duration._


object SpawnApp extends IOApp.Simple {
  def longRunningIO(): IO[Unit] =
    (
      IO.sleep(200.millis) *> IO.println(s"hi from thread ${Thread.currentThread()}").iterateWhile( _ => true)
      )

  def longRunningIORef(r: Ref[IO, Int]): IO[Unit] = (
    IO.sleep(200.millis) *> IO.println(s"hi from thread ${Thread.currentThread()}").iterateWhile( _ => true)
    )
  /*
    def run: IO[Unit] = for {
      fiber <- Spawn[IO].start(longRunningIO)
      _ <- IO.println("the fiber has been started")
      _ <- IO.sleep(1.second)
    } yield()
    */

  def run: IO[Unit] = for {
    r <- Ref.of[IO, Int](10)
    fiber1 <- Spawn[IO].start(longRunningIORef(r))
    fiber2 <- Spawn[IO].start(longRunningIO)
    fiber3 <- Spawn[IO].start(longRunningIO)
    _ <- IO.println("the fibers has been started")
    _ <- IO.sleep(2.second)
    _ <- fiber1.cancel
    _ <- fiber2.cancel
    _ <- IO.sleep(3.second)
  } yield()

}
