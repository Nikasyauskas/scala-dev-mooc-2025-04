package ru.otus.module3

import zio.{Clock, Console, Duration, IO, RIO, Random, Scope, Tag, Task, UIO, URIO, ZEnvironment, ZIO, durationInt}

import scala.language.postfixOps
import java.io.IOException

object di {

  type Query[_]
  type DBError
  type QueryResult[_]
  type Email = String

  trait User{
    def email: String
  }


  trait DBService{
    def tx[T](query: Query[T]): IO[DBError, QueryResult[T]]
  }

  trait EmailService{
    def makeEmail(email: String, body: String): Task[Email]
    def sendEmail(email: Email): Task[Unit]
  }

  trait LoggingService{
    def log(str: String): Task[Unit]
  }

  trait UserService{
      def getUserBy(id: Int): RIO[LoggingService, User]
      def id: Int
  }




  /**
   * Написать эффект, который напечатает в консоль приветствие, подождет 5 секунд,
   * сгенерит рандомно число, напечатает его в консоль
   *   Console
   *   Clock
   *   Random
   */


  lazy val e: ZIO[Random with Clock with Console, Nothing, Unit] = for{
    console <- ZIO.service[Console]
    clock <- ZIO.service[Clock]
    random <- ZIO.service[Random]
    _ <- console.printLine("Hello")
    _ <- clock.sleep(5 seconds)
    int <- random.nextInt()
    _ <- console.printLine(int.toString)
  } yield ()



  trait Console{
    def printLine(v: Any): UIO[Unit]
  }

  trait Clock{
    def sleep(duration: Duration): UIO[Unit]
  }

  trait Random{
    def nextInt(): UIO[Int]
  }




  type MyEnv = Random with Clock with Console

  lazy val e1: ZIO[MyEnv, Nothing, Unit] = e


  lazy val z1: URIO[Int, Unit] = ???   // f: Int => Unit
  lazy val z2: URIO[String, Unit] = ??? // f2: String => Unit

  // f3 : (Int, String) = (i, str) => {f(i); f2(str)}

  lazy val z3: ZIO[String with Int, Nothing, Unit] = z1 zipRight z2

  // def identity[A](a: A): A
  // def serviceWithZIO[A, B, E, R](f: A => ZIO[R, E, B]): ZIO[A with R, E, B]
  // def serviceWith[A, B, E, R](f: A => B): ZIO[A, E, B]
  // def service[A, B, E, R](f: A => B): ZIO[A, E, B] = serviceWith(identity)



  lazy val getUser: ZIO[LoggingService with UserService, Throwable, User] =
    ZIO.serviceWithZIO[UserService](_.getUserBy(10))


  lazy   val e3: URIO[Int, Unit] = ???
  lazy   val e4: URIO[String, Unit] = ???

  /**
   * Эффект, который будет комбинацией двух эффектов выше
   */

  lazy val e5 = ???



  /**
   * Написать ZIO программу, которая выполнит запрос и отправит email
   */




  lazy val services: ZEnvironment[UserService with EmailService with LoggingService] = ???

  lazy val dBService: DBService = ???
  lazy val userService: UserService = ???

  lazy val emailService2: EmailService = ???

  def f(userService: ZEnvironment[UserService]): ZEnvironment[UserService with EmailService with LoggingService] = ???


  lazy  val queryAndNotify: ZIO[LoggingService with EmailService with UserService, Throwable, Unit] = ???

  // provide
  lazy  val e6: IO[Throwable, Unit] = queryAndNotify.provideEnvironment(services)

  // provide some environment
  lazy  val e7: ZIO[UserService, Throwable, Unit] =
    queryAndNotify.provideSomeEnvironment[UserService](f)

  trait ToyScope {
    def close: UIO[Unit]
    def addFinalizer(f: => UIO[Any]): UIO[Unit]
  }

  def withFinalizer[R, E, A](zio: ZIO[R, E, A])(finalizer: A => UIO[Any]): ZIO[R with ToyScope, E, A] = {
    zio.flatMap{ a =>
      ZIO.serviceWithZIO[ToyScope](_.addFinalizer(finalizer(a))) *> ZIO.succeed(a)
    }
  }

  object ToyScope {

    private def buildEnv[R: Tag](e: ZEnvironment[R]): ZEnvironment[R with ToyScope] = {
      val toyScope = new ToyScope {
        val finalizers = scala.collection.mutable.ListBuffer.empty[UIO[Any]]
        override def close: UIO[Unit] = ZIO.collectAll(finalizers.toList).unit

        override def addFinalizer(f: => UIO[Any]): UIO[Unit] = ZIO.succeed(finalizers.addOne(f))
      }
      ZEnvironment(toyScope).++[R](e)
    }

    def toyScoped[R: Tag, E, A](zio: ZIO[R with ToyScope, E, A]): ZIO[R, E, A] =
      zio.flatMap(a => ZIO.serviceWithZIO[ToyScope](_.close) *> ZIO.succeed(a))
        .provideSomeEnvironment[R](zr => buildEnv[R](zr))
  }

  val z5: ZIO[ToyScope, Nothing, Unit] = withFinalizer(ZIO.succeed(println("hello world 1")))(_ => ZIO.succeed(println("Running finalizer 1")))
  val z6: ZIO[ToyScope, Nothing, Unit] = withFinalizer(ZIO.succeed(println("hello world 2")))(_ => ZIO.succeed(println("Running finalizer 2")))
  val z7: ZIO[ToyScope, Nothing, Unit] = z5 zipRight z6

  val z8: ZIO[Any, Nothing, Unit] = ToyScope.toyScoped[Any, Nothing, Unit](z7)


}