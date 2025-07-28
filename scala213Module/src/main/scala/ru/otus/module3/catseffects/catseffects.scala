package ru.otus.module3.catseffects

import cats.effect.unsafe.implicits.global
import cats.effect._
import cats.implicits._

import scala.concurrent.Future
import cats.{Functor, Monad, Monoid, Semigroup}
import cats.kernel.Group

import java.lang
/*
IO[A]
Resource[F[_], A]
Deffered[F[_], A], Ref[F, A]

Async[F[_]]
Sync[F[_]]
Concurrent[F[_]]


IO.pure(a)
IO.delay(thuk)

trait IO[A] {
  def run(): A
}

Monad
Applicative
Functor
MonadError
Async/Concurrent

val program: IO[Int] = IO {
  42
}.map(_ + 1)



 */


object catseffects {
  def main(args: Array[String]): Unit = {
    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
    val hello: IO[Unit] = IO(println("sdg"))
    hello.unsafeRunSync()

    val delay: IO[Int] = IO.delay {
      42
    }
    val pureValue = IO.pure(42)


    val optionalFunctor = Functor[Option]
    val res = optionalFunctor.map(Some(2))(_*3)

    // Semigroup combine
    // Monoid   comnine, empty
    // Group   combine, inverse
    // Functor

    val sum = Semigroup[Int].combine(2,3)

    val sum1 = Monoid[Int].combine(2,3)
    val sum1_empty = Monoid[Int].empty

    val fromEither = IO.fromEither(Left(new Exception("fail")))
    val fromFuture = IO.fromFuture(IO.delay(Future.successful(1)))

    val failing = IO.raiseError(new Exception("sadf"))
    val never = IO.never



  }
}


// example without taglessfinal

object FileAndHttpIO extends IOApp.Simple {
  def readFile(file: String): IO[String] =
    IO.pure("content of some file")

  def httpPost(url: String, body: String): IO[Unit] =
    IO.delay(println(s"Post $url : $body"))

  def run: IO[Unit] = for {
    _ <- IO.delay(println("enter file path"))
    path <- IO.readLine
    data <- readFile(path)
    _ <- httpPost("sdfsdf.de", data)
  } yield ()
}

// example with TG

//DSL
trait FileSystem[F[_]] {
  def readFile(path: String): F[String]
}

object FileSystem {
  def apply[F[_]: FileSystem]: FileSystem[F] = implicitly
}

trait HttpClient[F[_]] {
  def postData(url: String, body: String): F[Unit]
}

object HttpClient {
  def apply[F[_]: HttpClient]: HttpClient[F] = implicitly
}

trait Console[F[_]] {
  def readLine: F[String]
  def printLine(s: String): F[Unit]
}

object Console {
  def apply[F[_]: Console]: Console[F] = implicitly
}

object Interpreter {
  implicit val consoleIO: Console[IO] = new Console[IO] {
    override def readLine: IO[String] = IO.readLine

    override def printLine(s: String): IO[Unit] = IO.println(s)
  }

  implicit val fileSystemIO: FileSystem[IO] = new FileSystem[IO] {
    override def readFile(path: String): IO[String] = IO.pure(s"some file with content $path")
  }

  implicit val httpClientIO: HttpClient[IO] = new HttpClient[IO] {
    override def postData(url: String, body: String): IO[Unit] = IO.delay(println(s"Post $url: $body"))
  }
}

object FilesAndHttpTF extends IOApp.Simple {
  def program[F[_]: Console: Monad: FileSystem: HttpClient]: F[Unit] =
    for {
      _ <- Console[F].printLine("enter some path: ")
      path <- Console[F].readLine
      data <- FileSystem[F].readFile(path)
      _ <- HttpClient[F].postData("dsfsdf.de", data)
    } yield ()

  import Interpreter.{httpClientIO, fileSystemIO, consoleIO}

  def run:IO[Unit] = program[IO]
}
