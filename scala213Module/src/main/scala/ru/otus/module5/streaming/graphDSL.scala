package ru.otus.module5.streaming

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, ClosedShape, OverflowStrategy}
import akka.stream.scaladsl.{Broadcast, Flow, GraphDSL, RunnableGraph, Sink, Source, Zip}


object graphDSL {
  implicit val system = ActorSystem("sxdbvg")
  implicit val materializer = ActorMaterializer()

  val graph = GraphDSL.create() { implicit builder: GraphDSL.Builder[NotUsed] =>
    import GraphDSL.Implicits._

    //1
    val input = builder.add(Source(1 to 1000))
    val broadcast = builder.add(Broadcast[Int](2))
    val inc = builder.add(Flow[Int].map(_+1))
    val mult = builder.add(Flow[Int].map(_*10))
    val zip = builder.add(Zip[Int, Int])
    val output = builder.add(Sink.foreach[(Int, Int)](println))

    //2 shape

    input ~> broadcast
    broadcast.out(0) ~> inc ~> zip.in0
    broadcast.out(1) ~> mult ~> zip.in1
    zip.out ~> output

    //3 close shape
    ClosedShape
  }

  def main(args: Array[String]): Unit = {
    RunnableGraph.fromGraph(graph).run()
  }



}
