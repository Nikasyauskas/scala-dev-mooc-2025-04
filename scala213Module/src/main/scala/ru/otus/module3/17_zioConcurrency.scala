package ru.otus.module3

import zio.{Clock, Console, Executor, Ref, UIO, URIO, ZIO, durationInt}

import java.io.IOException
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import scala.language.postfixOps


object zioConcurrency {


  // эффект содержит в себе текущее время
  val currentTime: URIO[Clock, Long] = Clock.currentTime(TimeUnit.SECONDS)


  /**
   * Напишите эффект, который будет считать время выполнения любого эффекта
   */

    // 1. Получить время
    // 2. Включить эффект в цепочку
    // 3. Получить время
    // 4. Вывести разницу

    def printEffectRunningTime[R, E, A](zio: ZIO[R, E, A]): ZIO[R with Clock, E, A] = for{
      start <- currentTime
      r <- zio
      end <- currentTime
      _ <- ZIO.succeed(println(s"Running time: ${end - start}"))
    } yield r


  val exchangeRates: Map[String, Double] = Map(
    "usd" -> 76.02,
    "eur" -> 91.27
  )

  /**
   * Эффект который все что делает, это спит заданное кол-во времени, в данном случае 1 секунду
   */
  lazy val sleep1Second: UIO[Unit] = ZIO.sleep(1 seconds)

  /**
   * Эффект который все что делает, это спит заданное кол-во времени, в данном случае 3 секунды
   */
  lazy val sleep3Seconds: UIO[Unit] = ZIO.sleep(3 seconds)

  /**
   * Создать эффект, который печатает в консоль GetExchangeRatesLocation1 спустя 3 секунды
   */
  lazy val getExchangeRatesLocation1: UIO[Unit] = sleep3Seconds zipRight ZIO.succeed(println("GetExchangeRatesLocation1"))

  /**
   * Создать эффект, который печатает в консоль GetExchangeRatesLocation2 спустя 1 секунду
   */
  lazy val getExchangeRatesLocation2: UIO[Unit] = sleep1Second zipRight ZIO.succeed(println("GetExchangeRatesLocation2"))


  /**
   * Написать эффект, который получит курсы из обеих локаций
   */

   lazy val getFrom2Locations: UIO[Unit] = getExchangeRatesLocation1 zip getExchangeRatesLocation2


  /**
   * Написать эффект, который получит курсы из обеих локаций параллельно
   */

   lazy val getFrom2LocationsPar: UIO[Unit] = for{
     f1 <- getExchangeRatesLocation1.fork
     f2 <- getExchangeRatesLocation2.fork
     r1 <- f1.join
     r2 <- f2.join
   } yield (r1, r2)


  /**
   * Предположим нам не нужны результаты, мы сохраняем в базу и отправляем почту
   */


   lazy val writeUserToDB = sleep3Seconds zipRight ZIO.succeed("User saved")

   lazy val sendMail = sleep1Second zipRight ZIO.succeed("Mail sent")

  /**
   * Написать эффект, который сохранит в базу и отправит почту параллельно
   */

  lazy val writeAndSend = for{
    _ <- writeUserToDB.fork
    _ <- sendMail.fork
  } yield ()


  /**
   *  Greeter
   */

  lazy val greeter: ZIO[Any, Nothing, Nothing] = (sleep1Second zipRight ZIO.succeed(println("Hello"))) zipRight greeter

  def imperativeGreeter(cancelled: AtomicBoolean) = while (true && !cancelled.get()){
    println("Hello")
  }

  val g1 = for{
    f1 <- greeter.fork
    _ <- sleep3Seconds
    _ <- f1.interrupt
    _ <- f1.join
  } yield ()

  val g2 = for{
    ref <- ZIO.succeed(new AtomicBoolean(false))
    f1 <- ZIO.attemptBlockingCancelable(imperativeGreeter(ref))(ZIO.succeed(ref.set(true))).fork
    _ <- f1.interrupt
  } yield ()

  // 1. эффект не запущен
  // 2. на стыке композиции







  /***
   * Greeter 2
   * 
   * 
   * 
   */


 lazy val greeter2 = ???
  

  /**
   * Прерывание эффекта
   */

   lazy val app3 = ???





  /**
   * Получение информации от сервиса занимает 1 секунду
   */
  def getFromService(ref: Ref[Int]) = ???

  /**
   * Отправка в БД занимает в общем 5 секунд
   */
  def sendToDB(ref: Ref[Int]): ZIO[Clock, Exception, Unit] = ???


  /**
   * Написать программу, которая конкурентно вызывает выше описанные сервисы
   * и при этом обеспечивает сквозную нумерацию вызовов
   */

  
  lazy val app1 = ???

  /**
   *  Concurrent operators
   */

  val z1 = ZIO.sleep(1 seconds) zipRight ZIO.succeed(10)
  val z2 = ZIO.sleep(2 seconds) zipRight ZIO.succeed(20)
  val z3 = ZIO.sleep(2 seconds) zipRight ZIO.succeed("Hello")

  val r: ZIO[Any, Nothing, (Int, Int)] = z1 zipPar z2

  val r2: ZIO[Any, Nothing, Either[Int, String]] = z1 raceEither z3
  val r3: ZIO[Any, Nothing, Either[Int, String]] = z1 race z2 raceEither  z3

  val r4 = ZIO.foreachPar(List(1, 2, 3, 4, 5)){ i =>
    sleep1Second zipRight ZIO.succeed(println(i))
  }



  /**
   * Lock
   */


  // Правило 1
  lazy val doSomething: UIO[Unit] = ???
  lazy val doSomethingElse: UIO[Unit] = ???

  lazy val executor: Executor = ???

  lazy val eff = for{
    f1 <- doSomething.fork
    _ <- doSomethingElse
    r <- f1.join
  } yield r

  lazy val result = eff.onExecutor(executor)



  // Правило 2
  lazy val executor1: Executor = ???
  lazy val executor2: Executor = ???



  lazy val eff2 = for{
      f1 <- doSomething.onExecutor(executor2).fork
      _ <- doSomethingElse
      r <- f1.join
    } yield r

  lazy val result2 = eff2.onExecutor(executor)



}