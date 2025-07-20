package ru.otus.module3

import ru.otus.module3.emailService.EmailService
import ru.otus.module3.userDAO.UserDAO
import ru.otus.module3.userService.UserService
import ru.otus.module3.userService.UserID
import zio.Console.ConsoleLive
import zio.{Chunk, Console, RIO, URIO, Unsafe, ZEnvironment, ZIO, ZLayer}


object buildingZIOServices{


  val zioEnvironment: ZEnvironment[Console] =
    ZEnvironment[Console](Console.ConsoleLive)

  def main(args: Array[String]): Unit = {

    lazy val app: ZIO[UserService, Throwable, Unit] = UserService.notifyUser(UserID(1))

    val env: ZLayer[Any, Nothing, UserService] = ZLayer.make[UserService](UserService.live, UserDAO.live, EmailService.live)
    Unsafe.unsafe { implicit unsafe =>
      zio.Runtime.default.unsafe.run(
        app.provide(env)
      )
    }
  }
}