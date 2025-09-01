package ru.otus.module5.streaming
import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, OverflowStrategy}
import akka.stream.scaladsl.Source.actorRefWithAck
import akka.stream.scaladsl.{Flow, Sink, Source}

object backpressure extends  App{
  implicit val system = ActorSystem("sdg")
  implicit val materializer = ActorMaterializer()

  val fastSource = Source(1 to 1000)
  val flow = Flow[Int].map{el=>
    println(s"Flow inside: $el")
    el + 10
  }

  val flowWithBuffer = flow.buffer(10, overflowStrategy = OverflowStrategy.dropHead)
  val slowSink = Sink.foreach[Int]{el =>
    Thread.sleep(1000)
    println(s"sink inside: $el")
  }

  fastSource.async
    .via(flow).async
    .to(slowSink)
  //  .run()

  fastSource.async
    .via(flowWithBuffer).async
    .to(slowSink)
    .run()

}
