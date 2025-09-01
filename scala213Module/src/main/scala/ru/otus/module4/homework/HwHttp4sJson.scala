package ru.otus.module4.homework

import cats.effect.{IO, IOApp, Ref}
import cats.syntax.all._
import fs2.Stream
import io.circe.Json
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.ember.server.EmberServerBuilder
import com.comcast.ip4s._
import scala.concurrent.duration._



object HwHttp4sJson extends IOApp.Simple {

    case class CounterResponse(counter: Int)
    object CounterResponse {
      implicit val encoder = io.circe.generic.semiauto.deriveEncoder[CounterResponse]
    }

    case class SlowParams(chunk: Int, total: Int, time: Int)

    def counterRoutes(counterRef: Ref[IO, Int]): HttpRoutes[IO] = {
      HttpRoutes.of[IO] {
        case GET -> Root / "counter" =>
          for {
            current <- counterRef.get
            _ <- counterRef.set(current + 1)
            response <- Ok(CounterResponse(current + 1).asJson)
          } yield response
      }
    }

    def slowRoutes: HttpRoutes[IO] = {
      HttpRoutes.of[IO] {
        case GET -> Root / "slow" / chunk / total / time =>
          (chunk.toIntOption, total.toIntOption, time.toIntOption) match {
            case (Some(c), Some(t), Some(tm)) if c > 0 && t > 0 && tm > 0 =>
              val chunkSize = c
              val totalSize = t
              val delaySeconds = tm
              val content = Stream.emit("A").repeat.take(totalSize.toLong)
              val chunkedStream = content.chunkN(chunkSize).map { chunk =>
              chunk.toList.mkString
            }.metered[IO](delaySeconds.seconds)

              Ok(chunkedStream, Headers("Content-Type" -> "text/plain"))

            case _ =>
              BadRequest("Invalid parameters. All parameters must be positive integers.")
          }
      }
    }

    def allRoutes(counterRef: Ref[IO, Int]): HttpRoutes[IO] = {
      counterRoutes(counterRef) <+> slowRoutes
    }

    def run: IO[Unit] = {
      for {
        counterRef <- Ref.of[IO, Int](0)
        _ <- EmberServerBuilder
          .default[IO]
          .withHost(ipv4"0.0.0.0")
          .withPort(port"8080")
          .withHttpApp(allRoutes(counterRef).orNotFound)
          .build
          .use { server =>
            IO.println(s"Server started at ${server.address}") *>
              IO.never
          }
      } yield ()
    }
  }
  