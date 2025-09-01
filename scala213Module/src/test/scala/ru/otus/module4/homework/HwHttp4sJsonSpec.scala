package ru.otus.module4.homework

import cats.effect.{IO, Ref, unsafe}
import cats.syntax.all._
import io.circe.Json
import org.http4s._
import org.http4s.circe._
import org.http4s.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.duration._

import unsafe.implicits.global

class HwHttp4sJsonSpec extends AnyFlatSpec with Matchers {

  "HwHttp4sJson" should "increment counter on each request" in {
    val test = for {
      counterRef <- Ref.of[IO, Int](0)
      routes = HwHttp4sJson.allRoutes(counterRef)
      
      // First request
      req1 = Request[IO](Method.GET, uri"/counter")
      resp1 <- routes.orNotFound.run(req1)
      body1 <- resp1.as[Json]
      
      // Second request
      req2 = Request[IO](Method.GET, uri"/counter")
      resp2 <- routes.orNotFound.run(req2)
      body2 <- resp2.as[Json]
      
      // Third request
      req3 = Request[IO](Method.GET, uri"/counter")
      resp3 <- routes.orNotFound.run(req3)
      body3 <- resp3.as[Json]
    } yield {
      resp1.status shouldBe Status.Ok
      resp2.status shouldBe Status.Ok
      resp3.status shouldBe Status.Ok
      
      body1.hcursor.get[Int]("counter").toOption shouldBe Some(1)
      body2.hcursor.get[Int]("counter").toOption shouldBe Some(2)
      body3.hcursor.get[Int]("counter").toOption shouldBe Some(3)
    }
    
    test.unsafeRunSync()
  }

  it should "return BadRequest for invalid slow parameters" in {
    val test = for {
      counterRef <- Ref.of[IO, Int](0)
      routes = HwHttp4sJson.allRoutes(counterRef)
      
      // Test with non-numeric parameters
      req1 = Request[IO](Method.GET, uri"/slow/abc/def/ghi")
      resp1 <- routes.orNotFound.run(req1)
      
      // Test with negative numbers
      req2 = Request[IO](Method.GET, uri"/slow/-1/100/5")
      resp2 <- routes.orNotFound.run(req2)
      
      // Test with zero
      req3 = Request[IO](Method.GET, uri"/slow/0/100/5")
      resp3 <- routes.orNotFound.run(req3)
    } yield {
      resp1.status shouldBe Status.BadRequest
      resp2.status shouldBe Status.BadRequest
      resp3.status shouldBe Status.BadRequest
    }
    
    test.unsafeRunSync()
  }

  it should "return Ok for valid slow parameters" in {
    val test = for {
      counterRef <- Ref.of[IO, Int](0)
      routes = HwHttp4sJson.allRoutes(counterRef)
      
      // Test with valid parameters
      req = Request[IO](Method.GET, uri"/slow/10/100/1")
      resp <- routes.orNotFound.run(req)
    } yield {
      resp.status shouldBe Status.Ok
      resp.headers.get(org.typelevel.ci.CIString("Content-Type")).map(_.head.value) shouldBe Some("text/plain")
    }
    
    test.unsafeRunSync()
  }

  it should "handle counter and slow endpoints independently" in {
    val test = for {
      counterRef <- Ref.of[IO, Int](0)
      routes = HwHttp4sJson.allRoutes(counterRef)
      
      // Counter request
      counterReq = Request[IO](Method.GET, uri"/counter")
      counterResp <- routes.orNotFound.run(counterReq)
      counterBody <- counterResp.as[Json]
      
      // Slow request
      slowReq = Request[IO](Method.GET, uri"/slow/5/50/2")
      slowResp <- routes.orNotFound.run(slowReq)
    } yield {
      counterResp.status shouldBe Status.Ok
      slowResp.status shouldBe Status.Ok
      
      counterBody.hcursor.get[Int]("counter").toOption shouldBe Some(1)
    }
    
    test.unsafeRunSync()
  }
}


