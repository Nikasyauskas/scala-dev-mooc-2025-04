package ru.otus.module3.zio_homework

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault, Clock, ZLayer}
import ru.otus.module3.zio_homework.runApp

object ZioHomeWorkApp extends ZIOAppDefault{
  override def run = runApp.provide(TimingServiceLayer.live, ZLayer.succeed(Clock.ClockLive))

}
