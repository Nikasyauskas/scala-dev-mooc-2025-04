package ru.otus.module3.catsconcurrency.cats_effect_homework

import cats.effect.{IO, IOApp}
import cats.implicits._
import scala.concurrent.duration._

// Поиграемся с кошельками на файлах и файберами.

// Нужно написать программу где инициализируются три разных кошелька и для каждого из них работает фоновый процесс,
// который регулярно пополняет кошелек на 100 рублей раз в определенный промежуток времени. Промежуток надо сделать разный, чтобы легче было наблюдать разницу.
// Для определенности: первый кошелек пополняем раз в 100ms, второй каждые 500ms и третий каждые 2000ms.
// Помимо этих трёх фоновых процессов (подсказка - это файберы), нужен четвертый, который раз в одну секунду будет выводить балансы всех трех кошельков в консоль.
// Основной процесс программы должен просто ждать ввода пользователя (IO.readline) и завершить программу (включая все фоновые процессы) когда ввод будет получен.
// Итого у нас 5 процессов: 3 фоновых процесса регулярного пополнения кошельков, 1 фоновый процесс регулярного вывода балансов на экран и 1 основной процесс просто ждущий ввода пользователя.

// Можно делать всё на IO, tagless final тут не нужен.

// Подсказка: чтобы сделать бесконечный цикл на IO достаточно сделать рекурсивный вызов через flatMap:
// def loop(): IO[Unit] = IO.println("hello").flatMap(_ => loop())
object WalletFibersApp extends IOApp.Simple {

  def topupLoop(wallet: Wallet[IO], interval: Long): IO[Unit] =
    for {
      _ <- IO.sleep(interval.millis)
      _ <- wallet.topup(100)
      _ <- topupLoop(wallet, interval)
    } yield ()

  def balancePrinterLoop(wallet1: Wallet[IO], wallet2: Wallet[IO], wallet3: Wallet[IO]): IO[Unit] =
    for {
      _ <- IO.sleep(1.seconds)
      balance1 <- wallet1.balance
      balance2 <- wallet2.balance
      balance3 <- wallet3.balance
      _ <- IO.println(s"Wallet 1: ${balance1}₽, Wallet 2: ${balance2}₽, Wallet 3: ${balance3}₽")
      _ <- balancePrinterLoop(wallet1, wallet2, wallet3)
    } yield ()

  def run: IO[Unit] =
    for {
      _ <- IO.println("Press any key to stop...")
      wallet1 <- Wallet.fileWallet[IO]("1")
      wallet2 <- Wallet.fileWallet[IO]("2")
      wallet3 <- Wallet.fileWallet[IO]("3")
      fiber1 <- topupLoop(wallet1, 100).start
      fiber2 <- topupLoop(wallet2, 500).start
      fiber3 <- topupLoop(wallet3, 2000).start
      balanceFiber <- balancePrinterLoop(wallet1, wallet2, wallet3).start
      _ <- IO.readLine
      _ <- fiber1.cancel
      _ <- fiber2.cancel
      _ <- fiber3.cancel
      _ <- balanceFiber.cancel
      _ <- IO.println("Shutting down...")
    } yield ()

}
