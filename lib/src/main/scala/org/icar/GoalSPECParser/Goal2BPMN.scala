package org.icar.GoalSPECParser

import org.icar.bpmn2goal.Task
import org.icar.symbolic.{EndEvent, SequenceFlow}

import scala.xml.Elem

object Goal2BPMN {

  def BPMNHeader(): Elem = {
    <definitions xmlns:flowable="http://flowable.org/bpmn" xmlns="http://www.omg.org/spec/BPMN/20100524/MODEL"
                 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"
                 xmlns:activiti="http://activiti.org/bpmn" xmlns:bpmndi="http://www.omg.org/spec/BPMN/20100524/DI"
                 xmlns:omgdc="http://www.omg.org/spec/DD/20100524/DC" xmlns:omgdi="http://www.omg.org/spec/DD/20100524/DI"
                 typeLanguage="http://www.w3.org/2001/XMLSchema" expressionLanguage="http://www.w3.org/1999/XPath"
                 targetNamespace="http://www.activiti.org/test"></definitions>
  }

  def BPMNProcess(): Elem = {
    val processID = ""
    val processName = ""
    <process id={processID} name={processName} isExecutable="true"></process>
  }

  def extentionElements(): Elem = {
    <extensionElements>
      {executionListener()}
    </extensionElements>
  }

  def executionListener(): Elem = {
    val event = ""
    val className = ""
    <flowable:executionListener event={event} class={className}></flowable:executionListener>
  }

  def serviceTask(): Elem = {
    val name = "yeye"
    val taskID = ""
    val className = ""

    <serviceTask id={taskID} name={name} flowable:class={className}>
      {extentionElements}
    </serviceTask>
  }

  def userTask(name: String, taskID: String, candidateGroup: Option[String] = None): Elem = {
    <userTask id={name} name={taskID} flowable:candidateGroups={candidateGroup.getOrElse("")}></userTask>
  }

  def writeTask(t: Task): Elem = t.tasktype match {
    case "userTask" => userTask(t.label, t.id)
    case "serviceTask" => serviceTask()
  }

  //l = SequenceFlow(id.text, start, end, optional_condition)
  def seqFlow(s: SequenceFlow): Elem = {
    val flowID = java.util.UUID.randomUUID.toString
    val from = s.from
    val to = s.to
    <sequenceFlow id={flowID} sourceRef={from} targetRef={to}>
      {conditionalExpr(s.condition.toString)}
    </sequenceFlow>
  }

  def conditionalExpr(expression: String): Elem =
    scala.xml.XML.loadString(s"<conditionExpression xsi:type=\"tFormalExpression\">\n<![CDATA[\n{${expression}}\n]]>\n</conditionExpression>")


  def boundaryEvent(): Elem = {
    <boundaryEvent id="boundaryerror2" name="Error" attachedToRef="gestionnaire_inform_orgs">
      {errorEventDefinition()}
    </boundaryEvent>
  }

  def errorEventDefinition(): Elem = {
    <errorEventDefinition errorRef="REQUIRE_ORCHESTRATION"></errorEventDefinition>
  }

  def timerEventDefinition(): Elem = {
    val timeDuration = ""
    <timerEventDefinition>
      <timeDuration>
        {timeDuration}
      </timeDuration>
    </timerEventDefinition>
  }


  def startEvent(): Elem = {
    val eventName = ""
    val eventID = ""


    <startEvent id={eventID} name={eventName}></startEvent>
  }

  def endEvent(ev : EndEvent): Elem = <endEvent id={ev.id} name={ev.id}></endEvent>

  def parallelGateway(): Elem = {
    <parallelGateway id="parallelgateway1" name="Parallel Gateway"></parallelGateway>
  }

  def exclusiveGateway(): Elem = {
    <exclusiveGateway id="incident_solved_gateway" name="Incident solved"></exclusiveGateway>
  }


  def main(args: Array[String]): Unit = {
    val ll = conditionalExpr().toString()

    print(ll)
  }

}
