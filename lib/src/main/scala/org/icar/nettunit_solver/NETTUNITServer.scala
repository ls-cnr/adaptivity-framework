package org.icar.nettunit_solver

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Directives._
import net.liftweb.json.{DefaultFormats, JsonParser, Serialization}
import org.DEMO.{NETTUNITDefinitionsDEMO, Test_NETTUNIT_DEMO}
import org.icar.GoalSPECParser.NETTUNIT.NETTUNITParser
import org.icar.bpmn2goal.{ServiceTask, Task}
import org.icar.symbolic.{FormulaParser, GroundPredicate, Predicate, StateOfWorld}

import scala.io.StdIn

final case class InterventionRequest(emergencyPlanID: String, empName: String, requestDescription: String)

object NETTUNITServer {

  implicit val formats = DefaultFormats

  val bpmnProcessID = "NETTUNITProcess"

  val FlowableAddress = "localhost"
  val FlowablePort = "8080"

  val MUSAAddress = "localhost"
  val MUSAPort = 8081 //please, set different from rabbitMQ port, which by default is on 8080

  var current_state: StateOfWorld = StateOfWorld(List.empty)

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

  def deployToFlowable(processDefID: String, processBPMNDef: String): Unit = {
    //replace tee with a dummy variable...
    val teeSymbol = "\u22A4"
    val repl = "${myVariable}"
    val newProcessDef = processBPMNDef.replace(teeSymbol, repl)
    val resultDeploy = scalaj.http.Http(s"http://${FlowableAddress}:${FlowablePort}/NETTUNIT/deployProcess/${processDefID}")
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
              val the_model = processDefBPMN.split(":", 2)
              val processDefID = the_model(0)
              val processBPMN = the_model(1)
              deployToFlowable(processDefID, processBPMN)
              //deployToFlowable(processDefBPMN)
              complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, s"Process '${processDefID}' deployed to Flowable."))
            }
          }
        }
      },
      path("Goal2BPMN") {
        post {
          decodeRequest {
            // unmarshal as string
            entity(as[String]) { str =>
              val the_model = str.split(":", 2)
              val process_name = the_model(0)

              if (current_state.statements.isEmpty)
                current_state = NETTUNITDefinitionsDEMO.initial;
              val goalModel = NETTUNITParser.loadGoalModel(the_model(1))
              val bpmn_string = Test_NETTUNIT_DEMO.goalModelDEMO2BPMN(current_state, goalModel, process_name)
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
              Test_NETTUNIT_DEMO.failCapability(capabilityServiceClass)
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
      },
      path("StateOfWorld") {
        get {
          complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "Current state: " + current_state))
        }
      },
      path("UpdateStateOfWorld") {
        post {
          decodeRequest {
            // unmarshal as string
            entity(as[String]) { composite_string =>
              val string_array = composite_string.split('|')
              val op_type: String = string_array(0)
              val pred_string: String = string_array(1)
              val capabilityServiceClass: String = string_array(2)
              println(s":: Received predicate (Update) ${pred_string}")
              val parser = new FormulaParser();
              val parsed_predicate: parser.ParseResult[Predicate] = parser.parseAll(parser.predicate, pred_string)
              val predicate: Predicate = parsed_predicate.get
              val grounded_predicate: GroundPredicate = predicate.get_grounded.get

              op_type.toUpperCase match {
                case "ADD" =>
                  val updated_current_state: List[GroundPredicate] = grounded_predicate :: current_state.statements
                  current_state = StateOfWorld(updated_current_state)
                case "REMOVE" =>
                  val updated_current_state: List[GroundPredicate] = current_state.statements.filter(_ != grounded_predicate)
                  current_state = StateOfWorld(updated_current_state)
              }

              println(s"UPDATED STATE OF WORLD. Executed capability: $capabilityServiceClass")
              //Test_NETTUNIT.restoreCapability(capabilityServiceClass)
              complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "Updated state: " + current_state))
            }
          }
        }
      },
      path("StartedProcess") {
        post {
          decodeRequest {
            // unmarshal as string
            entity(as[String]) { capabilityServiceClass =>

              // aggiungo al registro degli stati del mondo il riferimento
              // al processo iniziato



              println(s"UPDATED STATE OF WORLD. Executed capability: $capabilityServiceClass")
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

