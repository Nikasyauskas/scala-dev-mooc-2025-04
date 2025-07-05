package ru.otus.module3

import ru.otus.module3.tryFinally.zioResource
import ru.otus.module3.tryFinally.zioResource.{closeDummyFile, handleDummyFile, handleFile, openDummyFile}
import zio.{Scope, Task, ZIO}

import java.io.IOException
import scala.concurrent.Future
import scala.io.{BufferedSource, Source}
import scala.language.postfixOps
import scala.util.{Failure, Success}

object tryFinally {

  object traditional {


    def acquireResource: Resource = Resource("Some resource")

    def use(r: Resource): Unit = println(s"Using resource: ${r.name}")

    def releaseResource(r: Resource): Unit  = r.close()

    /**
     * Напишите код, который обеспечит корректную работу с ресурсом:
     * получить ресурс -> использовать -> освободить
     *
     */

    lazy val result = {
      val r = acquireResource
      try{
        use(r)
      } finally {
        releaseResource(r)
      }
    }

    /**
     *
     * Обобщенная версия работы с ресурсом
     */

    def withResource[R, A](resource: => R)(release: R => Any)(use: R => A): A = {
      val r = resource
      try{
        use(r)
      } finally {
        release(r)
      }
    }

    lazy val result2 = withResource(acquireResource)(releaseResource)(use)



    /**
     * Прочитать строки из файла
     */

  }

  object future{
    implicit val global = scala.concurrent.ExecutionContext.global

    def acquireFutureResource: Future[Resource] = Future(Resource("Future resource"))

    def use(resource: Resource): Future[Unit] = Future(traditional.use(resource))

    def releaseFutureResource(resource: Resource): Future[Unit] =
      Future(traditional.releaseResource(resource))

    /**
     * Написать вспомогательный оператор ensuring, который позволит корректно работать
     * с ресурсами в контексте Future
     *
     */

    implicit class FutureOps[A](future: Future[A]) {
      def ensuring(finalizer: Future[Any]): Future[A] =
        future.transformWith {
          case Failure(exception) => finalizer.flatMap(_ => Future.failed(exception))
          case Success(value) => finalizer.flatMap(_ => Future.successful(value))
        }
    }

    val futureRes: Future[Unit] =
      acquireFutureResource.flatMap(r => use(r).ensuring(releaseFutureResource(r)))



    /**
     * Написать код, который получит ресурс, воспользуется им и освободит
     */

    lazy val futureResult = ???



  }

  object zioResource{


    /**
     * Реализовать ф-цию, которая будет описывать открытие файла с помощью ZIO эффекта
     */
    def openFile(fileName: String): Task[BufferedSource] = ZIO.attempt(Source.fromFile(fileName))

    def openDummyFile(fileName: String): Task[Resource] = ZIO.attempt(Resource(fileName))
    /**
     * Реализовать ф-цию, которая будет описывать закрытие файла с помощью ZIO эффекта
     */

    def closeFile(file: Source): Task[Unit] = ZIO.attempt(file.close())

    def closeDummyFile(file: Resource): Task[Unit] = ZIO.attempt(file.close())

    /**
     * Написать эффект, который прочитает строчки из файла и выведет их в консоль
     */

    def handleFile(file: Source) = ZIO.foreach(file.getLines().toList){ str =>
      ZIO.attempt(println(str))
    }

    def handleDummyFile(file: Resource) = ZIO.attempt(println(s"Using ${file.name}"))


    val r1: Task[Unit] = ZIO.acquireReleaseWith(openDummyFile("ZIO Resource"))(r =>
      ZIO.attempt(r.close()).orDie)(handleDummyFile)

    lazy val r2 = ???

    /**
     * Написать эффект, который откроет 2 файла, прочитает из них строчки,
     * выведет их в консоль и корректно закроет оба файла
     */

    lazy val r3 = ZIO.acquireReleaseWith(openDummyFile("Z Resource 1"))(closeDummyFile(_).orDie){ r1 =>
      ZIO.acquireReleaseWith(openDummyFile("Z Resource 2"))(closeDummyFile(_).orDie){ r2 =>
        handleDummyFile(r1) zipRight handleDummyFile(r2)
      }
    }



    /**
     * Рефакторинг выше написанного кода
     *
     */

    def withFile = ???


    lazy val twoFiles2 = ???

  }

}


object zioScope{



  /**
   * Написать эффект открывающий / закрывающий первый файл
   */
  lazy val file1: ZIO[Any with Scope, Throwable, Resource] =
    ZIO.acquireRelease(openDummyFile("Z Scope Resource 1"))(closeDummyFile(_).orDie)

  /** Написать эффект открывающий / закрывающий второй файл
    *
   */
  lazy val file2: ZIO[Any with Scope, Throwable, Resource] =
    ZIO.acquireRelease(openDummyFile("Z Scope Resource 2"))(closeDummyFile(_).orDie)


  /**
   * Использование ресурсов
   */

  val fileCombined: ZIO[Any with Scope, Throwable, (Resource, Resource)] = (file1 zipPar file2).parallelFinalizers

  /**
   * Написать эффект, который воспользуется ф-ей handleFile из блока про bracket
   * для печати строчек в консоль
   */

   val r1: ZIO[Any with Scope, Throwable, Unit] = fileCombined.flatMap{case (f1, f2) =>
    handleDummyFile(f1) zipRight handleDummyFile(f2)
   }

   val r2: ZIO[Any, Throwable, Unit] = ZIO.scoped(r1)


  val testTxt: ZIO[Scope, Throwable, BufferedSource] =
    ZIO.fromAutoCloseable(ZIO.attempt(Source.fromFile("test1.txt")))

  val testTxt2 = ZIO.attempt(Source.fromFile("test1.txt"))
    .withFinalizer(s => ZIO.succeed(s.close()))
     /**
      * Комбинирование ресурсов
      */



     // Комбинирование



  /**
   * Написать эффект, который прочитает и выведет строчки из обоих файлов
   */





  /**
   * Множество ресурсов
   */

  lazy val fileNames: List[String] = List(
    "Scope R1",
    "Scope R2",
    "Scope R3",
    "Scope R4",
    "Scope R5",
    "Scope R6",
    "Scope R7",
    "Scope R8",
    "Scope R9",
    "Scope R10"
  )

  def file(name: String): ZIO[Any with Scope, Throwable, BufferedSource] =
    ZIO.attempt(Source.fromFile(name)).withFinalizerAuto

  def file2(name: String) = ZIO.acquireRelease(ZIO.attempt(Resource(name)))(r =>
    ZIO.succeed(r.close()))


  // множественное открытие / закрытие
  lazy val files: ZIO[Scope, Throwable, List[BufferedSource]] = ZIO.foreach(fileNames){ fn =>
    file((fn))
  }

  lazy val files2: ZIO[Scope, Throwable, List[Resource]] = ZIO.foreach(fileNames){ fn =>
    file2((fn))
  }

  lazy val cc: ZIO[Scope, Throwable, List[Unit]] = files2.flatMap{ files =>
    ZIO.foreach(files){ file =>
      ZIO.attempt(println(s"Using ${file.name}"))
    }
  }




  // Использование


  // обработать N файлов



  lazy val files3: ZIO[Any with Scope, IOException, List[Source]] = ???

  /**
   * Прочитать строчки из файлов и вернуть список этих строк используя files3
   */
  lazy val r3: Task[List[String]] = ???
  


  val eff1: Task[BufferedSource] = ZIO.attempt(Source.fromFile("test.txt"))



  type Transactor

  def mkTransactor(c: Config): ZIO[Any with Scope, Throwable, Transactor] = ???

  type Config
  lazy val config: Task[Config] = ???

  lazy val m2 = ???

}