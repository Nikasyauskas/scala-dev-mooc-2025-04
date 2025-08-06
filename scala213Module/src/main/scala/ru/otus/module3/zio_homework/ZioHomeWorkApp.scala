package ru.otus.module3.zio_homework

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault, Clock, ZLayer}
import ru.otus.module3.zio_homework.appSpeedUp

object ZioHomeWorkApp extends ZIOAppDefault{
  override def run = appSpeedUp.provide(ZLayer.succeed(Clock.ClockLive))

  // .provide(ZLayer.succeed(Clock.ClockLive))
}
