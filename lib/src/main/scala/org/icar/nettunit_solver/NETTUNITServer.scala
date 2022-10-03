package org.icar.nettunit_solver

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._

import scala.io.StdIn

object NETTUNITServer {
  val MUSAAddress = "localhost"
  val MUSAPort = 8081 //please, set different from rabbitMQ port, which by default is on 8080

  def main(args: Array[String]): Unit = {

    implicit val system = ActorSystem("nettunit")
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher
    val route = concat(
      path("Deploy") {
        post {
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "Process deployed to Flowable."))
        }
      },
      path("Goal2BPMN") {
        get {
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "Converted"))
        }
      }
    )

    val bindingFuture = Http().newServerAt(MUSAAddress, MUSAPort).bind(route)

    println(s"MUSA Server now online. Please POST to http://${MUSAAddress}:${MUSAPort}/deploy to deploy a process to Flowable.\nPress RETURN to stop...")
    StdIn.readLine() // let it run until user presses return
    bindingFuture
      .flatMap(_.unbind()) // trigger unbinding from the port
      .onComplete(_ => system.terminate()) // and shutdown when done
  }
}

