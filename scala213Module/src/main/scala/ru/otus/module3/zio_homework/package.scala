package ru.otus.module3

import scala.language.postfixOps
import zio.{Console, Random, Clock}
import ru.otus.module3.zio_homework.config.{AppConfig, Configuration}
import ru.otus.module3.zioConcurrency.printEffectRunningTime
import zio._

import java.io.IOException
import java.util.concurrent.TimeUnit



package object zio_homework {
  /**
   * 1.
   * Используя сервисы Random и Console, напишите консольную ZIO программу которая будет предлагать пользователю угадать число от 1 до 3
   * и печатать в консоль угадал или нет. Подумайте, на какие наиболее простые эффекты ее можно декомпозировать.
   */



  lazy val guessProgram: IO[IOException, Unit] = for {
    rnd <- Random.nextIntBetween(1,4)
    _ <- Console.printLine("Enter digit between 1 and 3")
    numberFromConsole <- Console.readLine.map(_.toInt)
    _ <- if(rnd == numberFromConsole) Console.printLine("You guess") else Console.printLine("You Lose")
  } yield ()

  /**
   * 2. реализовать функцию doWhile (общего назначения), которая будет выполнять эффект до тех пор, пока его значение в условии не даст true
   * 
   */

  def doWhile[R, E, A](effect: ZIO[R, E, A])(condition: A => Boolean): ZIO[R, E, A] = {
    def loop: ZIO[R, E, A] = effect.flatMap { result =>
      if (condition(result)) ZIO.succeed(result)
      else loop
    }
    loop
  }

  lazy val doWhileChecker: ZIO[Any, IOException, Unit] = doWhile(Console.printLine("hello"))(_ => false)

  /**
   * 3. Реализовать метод, который безопасно прочитает конфиг из переменных окружения, а в случае ошибки вернет дефолтный конфиг
   * и выведет его в консоль
   * Используйте эффект "Configuration.config" из пакета config
   */

  def loadConfigOrDefault: IO[Exception, AppConfig] = for {
    appConf <- Configuration.config.catchAll(error =>
      Console.printLine(s"Application config not found. Applying default configuration") // too long 'error' for log
        *> Configuration.defaultConfig
    )
    _ <- Console.printLine(appConf)
  } yield appConf

  /**
   * 4. Следуйте инструкциям ниже для написания 2-х ZIO программ,
   * обратите внимание на сигнатуры эффектов, которые будут у вас получаться,
   * на изменение этих сигнатур
   */


  /**
   * 4.1 Создайте эффект, который будет возвращать случайным образом выбранное число от 0 до 10 спустя 1 секунду
   * Используйте сервис zio Random
   */
  lazy val eff: UIO[Int] = Random.nextIntBetween(0, 11).delay(1.second)


  /**
   * 4.2 Создайте коллукцию из 10 выше описанных эффектов (eff)
   */
  lazy val effects: UIO[List[Int]] = ZIO.collectAll(List.fill(10)(eff))


  /**
   * 4.3 Напишите программу которая вычислит сумму элементов коллекции "effects",
   * напечатает ее в консоль и вернет результат, а также залогирует затраченное время на выполнение,
   * можно использовать ф-цию printEffectRunningTime, которую мы разработали на занятиях
   */

  lazy val app: ZIO[Clock, IOException, Int] = for {
    result <- printEffectRunningTime(
      effects.flatMap(
        col => ZIO.succeed(col.sum)
      )
    )
    _ <- Console.printLine(result)
  } yield result



  /**
   * 4.4 Усовершенствуйте программу 4.3 так, чтобы минимизировать время ее выполнения
   */

  lazy val effectsPar: UIO[List[Int]] = ZIO.collectAllPar(List.fill(10)(eff))

  lazy val appSpeedUp: ZIO[Clock, IOException, Int] = for {
    result <- printEffectRunningTime(
      effectsPar.flatMap(
        col => ZIO.succeed(col.sum)
      )
    )
    _ <- Console.printLine(result)
  } yield result


  /**
   * 5. Оформите ф-цию printEffectRunningTime разработанную на занятиях в отдельный сервис, так чтобы ее
   * можно было использовать аналогично zio.Console.printLine например
   */
  
  trait TimingService {
    def timeEffect[R, E, A](effect: ZIO[R, E, A]): ZIO[R with Clock, E, A]
  }

  object TimingService {
    def timeEffect[R, E, A](effect: ZIO[R, E, A]): ZIO[R with Clock, E, A] = for {
      start <- Clock.currentTime(TimeUnit.SECONDS)
      result <- effect
      end <- Clock.currentTime(TimeUnit.SECONDS)
      _ <- ZIO.succeed(println(s"Running time: ${end - start}"))
    } yield result
  }

  case class TimingServiceLive() extends TimingService {
    override def timeEffect[R, E, A](effect: ZIO[R, E, A]): ZIO[R with Clock, E, A] = 
      TimingService.timeEffect(effect)
  }

  object TimingServiceLayer {
    val live: ZLayer[Any, Nothing, TimingService] = 
      ZLayer.succeed(TimingServiceLive())
  }


   /**
     * 6.
     * Воспользуйтесь написанным сервисом, чтобы создать эффект, который будет логировать время выполнения программы из пункта 4.3
     *
     * 
     */

  lazy val appWithTimeLogg: ZIO[Clock with TimingService, IOException, Int] = for {
    timingService <- ZIO.service[TimingService]
    result <- timingService.timeEffect(app)
  } yield result

  /**
    * 
    * Подготовьте его к запуску и затем запустите воспользовавшись ZioHomeWorkApp
    */
  lazy val runApp: ZIO[Clock with TimingService, IOException, Int] = appWithTimeLogg.provide(TimingServiceLayer.live, ZLayer.succeed(Clock.ClockLive))

}
