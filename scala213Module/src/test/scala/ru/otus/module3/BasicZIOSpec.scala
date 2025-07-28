package ru.otus.module3

import ru.otus.module3.emailService.EmailAddress
import ru.otus.module3.userService.{User, UserID}
import ru.otus.module3.zioDS
import zio.{Clock, Console, Random, ZIO, ZLayer, durationInt}
import zio.test.Assertion.{anything, contains, dies, diesWithA, equalTo, fails, hasFirst, hasSize, isSubtype, throws}
import zio.test.TestAspect.nonFlaky
import zio.test.{Gen, TestClock, TestConsole, ZIOSpecDefault, assert, assertZIO, check}

import scala.language.postfixOps



object BasicZIOSpec extends ZIOSpecDefault{

  val env = ZLayer.make[Random with Console with Clock](
    ZLayer.succeed(Random.RandomLive),
    ZLayer.succeed(zio.Console.ConsoleLive),
    ZLayer.succeed(zio.Clock.ClockLive))

  val greeter = for{
    _ <- zio.Console.printLine("Как тебя зовут")
    name <- zio.Console.readLine
    _ <- zio.Console.printLine(s"Привет, $name")
    age <- zio.Console.readLine
    _ <- zio.Console.printLine(s"Age $age")
  } yield ()



  // generation
  val intGen: Gen[Random, Int] = Gen.int
  val userIdGen: Gen[Random, UserID] = intGen.map(i => UserID(i))
  val emailGen: Gen[Any, EmailAddress] = Gen.alphaNumericStringBounded(3, 5).map(str => EmailAddress(s"${str}@mail.com"))
  val userGen: Gen[Random, User] = for{
    id <- userIdGen
    email <- emailGen
  } yield User(id, email)



  override def spec = suite("Basic")(
    suite("Arithmetic")(
      test("2 * 2")(
        assert(2 * 2) (equalTo(4))
      ),
      test("division by zero")(
        assert(2 / 0)(throws(isSubtype[ArithmeticException](anything)))
      )
    ),
    suite("effect testing")(
      test("simple effect")(
        assertZIO(ZIO.succeed(2 * 2))(equalTo(4))
      ),
      test("test console")(
        for{
          _ <- TestConsole.feedLines("Alex", "18")
          _ <- greeter
          value <- TestConsole.output
        } yield assert(value)(hasFirst(equalTo("Как тебя зовут\n")) && hasSize(equalTo(3)))
      ),
      test("test concurrency")(
        for{
          fiber <- (zio.Console.printLine("Hello") *> ZIO.sleep(5 seconds)).fork
          _ <- TestClock.adjust(5 seconds)
          _ <- fiber.join
          value <- TestConsole.output
        } yield assert(value(0))(equalTo("Hello\n"))
      ),
      test("test failing zio")(
        assertZIO(ZIO.succeed(2 / 0).exit)(diesWithA[ArithmeticException])
      )
    ),
    suite("property base testing")(
      test("int addition is associative")(
        check(intGen, intGen, intGen){ case (x, y, z) =>
          val left  = (x + y) + z
          val right = x + (y + z)
          assert(left)(equalTo(right))
        }
      )
    ),
    suite("concurrent update")(
      test("updateCounter")(
        assertZIO(zioDS.ref.updateCounterRef)(equalTo(3))
      ) @@nonFlaky
    )
  ).provide(env)

}
