package org.icar.nettunit_solver

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import net.liftweb.json.{DefaultFormats, JsonParser, Serialization}
import org.icar.GoalSPECParser.NETTUNIT.NETTUNITParser
import org.icar.bpmn2goal.{ServiceTask, Task}

import scala.io.StdIn

final case class InterventionRequest(emergencyPlanID: String, empName: String, requestDescription: String)

object NETTUNITServer {

  implicit val formats = DefaultFormats

  val bpmnProcessID = "NETTUNITProcess"

  val FlowableAddress = "localhost"
  val FlowablePort = "8080"

  val MUSAAddress = "localhost"
  val MUSAPort = 8081 //please, set different from rabbitMQ port, which by default is on 8080

  def executeProcess(opeartorName: String, requestDescription: String): Unit = {

    val body = s"{\n  \"emergencyPlanID\":\"$bpmnProcessID\",\n  \"empName\":\"${opeartorName}\",\n  \"requestDescription\":\"${requestDescription}\"\n}"
    val resultApply = scalaj.http.Http(s"http://${FlowableAddress}:${FlowablePort}/NETTUNIT/incident/apply")
      .postData(body)
      .header("Content-Type", "application/json").asString
  }

  def deployToFlowable(processBPMNDef: String): Unit = {
    //replace tee with a dummy variable...
    val teeSymbol = "\u22A4"
    val repl = "${myVariable}"
    val newProcessDef = processBPMNDef.replace(teeSymbol, repl)
    val resultDeploy = scalaj.http.Http(s"http://${FlowableAddress}:${FlowablePort}/NETTUNIT/deployProcess/${bpmnProcessID}")
      .postData(newProcessDef)
      .header("Content-Type", "application/xml").asString
  }

  def main(args: Array[String]): Unit = {

    implicit val system = ActorSystem("nettunit")
    // needed for the future flatMap/onComplete in the end
    implicit val executionContext = system.dispatcher
    val route = concat(
      path("Execute") {
        post {
          decodeRequest {
            // unmarshal as string
            entity(as[String]) { interventionRequestJSon =>
              val parsed = JsonParser.parse(interventionRequestJSon)
              val entity = parsed.extract[InterventionRequest]
              executeProcess(entity.empName, entity.requestDescription)
              complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s"Process ${entity.emergencyPlanID} execution started in Flowable."))
            }
          }
        }
      },
      path("Deploy") {
        post {
          decodeRequest {
            // unmarshal as string
            entity(as[String]) { processDefBPMN =>
              deployToFlowable(processDefBPMN)
              complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "Process deployed to Flowable."))
            }
          }
        }
      },
      path("Goal2BPMN") {
        post {
          decodeRequest {
            // unmarshal as string
            entity(as[String]) { str =>
              val goalModel = NETTUNITParser.loadGoalModel(str)
              val bpmn_string = Test_NETTUNIT.goalModel2BPMN(goalModel)
              val teeSymbol = "\u22A4" //true
              val teeDownSymbol = "\u22A5" //false
              var correct_bpmn_def = bpmn_string.replace(teeSymbol, "myVariable")
              correct_bpmn_def = correct_bpmn_def.replace(teeDownSymbol, "!myVariable")

              complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, correct_bpmn_def))
            }
          }
        }
      },
      path("FailCapability") {
        post {
          decodeRequest {
            // unmarshal as string
            entity(as[String]) { capabilityServiceClass =>
              println(s"FAILED SERVICE: $capabilityServiceClass")
              Test_NETTUNIT.failCapability(capabilityServiceClass)
              complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "Failed task: " + capabilityServiceClass))
            }
          }
        }
      },
      path("RestoreCapability") {
        post {
          decodeRequest {
            // unmarshal as string
            entity(as[String]) { capabilityServiceClass =>
              println(s"RESTORED SERVICE: $capabilityServiceClass")
              Test_NETTUNIT.restoreCapability(capabilityServiceClass)
              complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "Failed task: " + capabilityServiceClass))
            }
          }
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

