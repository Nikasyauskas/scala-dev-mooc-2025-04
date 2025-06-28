package ru.otus.module3

import zio._

import scala.concurrent.Future
import scala.io.StdIn
import scala.language.postfixOps
import scala.util.{Failure, Success, Try}



/** **
 * ZIO[-R, +E, +A] ----> R => Either[E, A]
 *
 */


object toyModel {


  /**
   * Используя executable encoding реализуем свой zio
   */

  case class ZIO[-R, +E, +A](run: R => Either[E, A]) {


    def map[B](f: A => B): ZIO[R, E, B] =
      flatMap(a => ZIO(_ => Right(f(a))))

    def flatMap[R1 <: R, E1 >: E, B](f: A => ZIO[R1, E1, B]): ZIO[R1, E1, B] =
      ZIO(r => this.run(r).fold(e => ZIO.fail(e),
        a => f(a)).run(r))
  }


  /**
   * Реализуем конструкторы под названием effect и fail
   */

  object ZIO {
    def effect[A](f: => A): ZIO[Any, Throwable, A] = try{
      ZIO(_ => Right(f))
    } catch {
      case e: Throwable => fail(e)
    }

    def fail[E](e: E): ZIO[Any, E, Nothing] = ZIO(_ => Left(e))
  }


  /** *
   * Напишите консольное echo приложение с помощью нашего игрушечного ZIO
   */

    val readLine: ZIO[Any, Throwable, String] = ZIO.effect(StdIn.readLine())
    def printLine(str: String): ZIO[Any, Throwable, Unit] = ZIO.effect(println(str))

    val echo: ZIO[Any, Throwable, Unit] = readLine.flatMap(printLine)






  type Error
  type Environment



  lazy val _: Task[Int] = ??? // ZIO[Any, Throwable, Int]
  lazy val _: IO[Error, Int] = ??? // ZIO[Any, Error, Int]
  lazy val _: RIO[Environment, Int] = ??? // ZIO[Env, Throwable, Int]
  lazy val _: URIO[Environment, Int] = ??? // ZIO[Env, Nothing, Int]
  lazy val _: UIO[Int] = ??? // ZIO[Any, Nothing, Int]
}

object zioConstructors {


  // не падающий эффект
  val z1: UIO[Int] = ZIO.succeed(7)

  val z2: Task[Int] = ZIO.attempt(10)

  val z3: IO[String, Nothing] = ZIO.fail("Some error")

  val future: Future[String] = Future.successful("Hello world")
//  import scala.concurrent.ExecutionContext.Implicits.global
//  val z4: ZIO[Any, Throwable, String] = ZIO.async{ cb =>
//    future.onComplete {
//      case Failure(exception) => cb(ZIO.fail(exception))
//      case Success(value) => cb(ZIO.succeed(value))
//    }
//  }

  // Из Future
  lazy val f: Future[Int] = ???
  val z5: Task[Int] = ZIO.fromFuture(implicit ec =>
    f.flatMap(r => Future(r + 1)))


  // Из try
  lazy val t: Try[String] = ???
  val z6: Task[String] = ZIO.fromTry(t)



  // Из option
  lazy val opt : Option[Int] = ???
  val z7: IO[Option[Nothing], Int] = ZIO.fromOption(opt)
  val z8: UIO[Option[Int]] = z7.option
  val z9: IO[Option[Nothing], Int] = z8.some


  type User
  type Address


  def getUser(): Task[Option[User]] = ???
  def getAddress(u: User): Task[Option[Address]] = ???

  val r = for{
    user <- getUser().some
    address <- getAddress(user)
  } yield address



  // Из either
  lazy val e: Either[String, Int] = ???
  val z10: IO[String, Int] = ZIO.fromEither(e)
  val z11: UIO[Either[String, Int]] = z10.either
  val z12: IO[String, Int] = z11.absolve



  // особые версии конструкторов

  val _ = ZIO.unit
  val _ = ZIO.none
  val _ = ZIO.never // while(true)
  val _ = ZIO.die(new Throwable("Oopps"))


}



object zioOperators {

  /** *
   *
   * 1. Создать ZIO эффект, который будет читать строку из консоли
   */

  lazy val readLine = ???

  /** *
   *
   * 2. Создать ZIO эффект, который будет писать строку в консоль
   */

  def writeLine(str: String) = ???

  /** *
   * 3. Создать ZIO эффект котрый будет трансформировать эффект содержащий строку в эффект содержащий Int
   */

  lazy val lineToInt = ???
  /** *
   * 3. Создать ZIO эффект, который будет работать как echo для консоли
   *
   */

  lazy val echo = ???

  /**
   * Создать ZIO эффект, который будет приветствовать пользователя и говорить, что он работает как echo
   */

  lazy val greetAndEcho = ???



  // greet and echo улучшенный
  lazy val _: ZIO[Any, Throwable, Unit] = ???


  /**
   * Используя уже созданные эффекты, написать программу, которая будет считывать поочереди считывать две
   * строки из консоли, преобразовывать их в числа, а затем складывать их
   */

  lazy val r1 = ???

  /**
   * Второй вариант
   */

  lazy val r2: ZIO[Any, Throwable, Int] = ???

  /**
   * Доработать написанную программу, чтобы она еще печатала результат вычисления в консоль
   */

  lazy val r3 = ???


  lazy val a: Task[Int] = ???
  lazy val b: Task[String] = ???

  /**
   * Последовательная комбинация эффектов a и b
   */
  lazy val ab1: ZIO[Any, Throwable, (Int, String)] = a zip b

  /**
   * Последовательная комбинация эффектов a и b
   */
  lazy val ab2: ZIO[Any, Throwable, String] = a zipRight b

  /**
   * Последовательная комбинация эффектов a и b
   */
  lazy val ab3: ZIO[Any, Throwable, Int] = a zipLeft b


  /**
   * Последовательная комбинация эффекта b и b, при этом результатом должна быть конкатенация
   * возвращаемых значений
   */
  lazy val ab4 = a.zipWith(b)(_ + _)




  /**
    * 
    * A as B
    */

  lazy val c = ???

}
