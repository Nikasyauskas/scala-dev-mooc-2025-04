package ru.otus.module3.catsresources
import cats.effect.{IO, IOApp, Resource}

object CatsResourceFakeExample extends IOApp.Simple {

  // Имитация ресурса чтения строки
  def readerResource(path: String): Resource[IO, String] =
    Resource.make(
      IO.pure("Это первая строка файла") // Имитация открытия ресурса
    )(_ => IO(println("Ресурс закрыт"))) // Имитация закрытия ресурса

  def readFirstLine(path: String): IO[String] =
    readerResource(path).use { line =>
      IO.pure(line) // просто возвращаем строку
    }

  override def run: IO[Unit] = {
    val filePath = "fake.txt"
    readFirstLine(filePath).flatMap {
      case null => IO.println("Файл пустой или ошибка чтения")
      case line => IO.println(s"Первая строка файла: $line")
    }
  }
}
