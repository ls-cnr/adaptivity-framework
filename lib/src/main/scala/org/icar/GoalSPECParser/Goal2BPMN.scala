package org.icar.GoalSPECParser

import org.icar.bpmn2goal._
import org.icar.symbolic.{SequenceFlow, _}

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
    <process id={processID} name={processName} isExecutable="true">
      {/*?*/}
    </process>
  }

  /*Service task------------------------------------------*/
  def serviceTask(name: String, taskID: String, className: String, extElems: Option[FlowableExtentionElements]): Elem = {
    <serviceTask id={taskID} name={name} flowable:class={className}>
      {if (extElems.isDefined) {
      extentionElements(extElems.get)
    }}
    </serviceTask>
  }

  def extentionElements(elems: FlowableExtentionElements): Elem = {
    <extensionElements>
      {elems.listeners.foreach(executionListener)}
    </extensionElements>
  }

  def executionListener(listener: FlowableExecutionListener): Elem = {
    <flowable:executionListener event={listener.event} class={listener.className}></flowable:executionListener>
  }
  /*------------------------------------------*/

  def userTask(name: String, taskID: String, candidateGroup: Option[String] = None): Elem = {
    <userTask id={name} name={taskID} flowable:candidateGroups={candidateGroup.getOrElse("")}></userTask>
  }

  def writeTask(item: Item): Elem = item match {
    case t: Task => userTask(t.label, t.id)
    case t: ServiceTask => serviceTask(t.label, t.id, t.className, t.extElems)
  }

  //l = SequenceFlow(id.text, start, end, optional_condition)
  def seqFlow(s: SequenceFlow): Elem = {
    val flowID = java.util.UUID.randomUUID.toString
    val from = s.from
    val to = s.to
    <sequenceFlow id={flowID} sourceRef={from.toString} targetRef={to.toString}>
      {conditionalExpr(s.condition.toString)}
    </sequenceFlow>
  }

  def conditionalExpr(expression: String): Elem =
    scala.xml.XML.loadString(s"<conditionExpression xsi:type=\"tFormalExpression\">\n<![CDATA[\n{${expression}}\n]]>\n</conditionExpression>")

  def boundaryEvent(evt: BoundaryEvent): Elem = {
    <boundaryEvent id={evt.id} name={evt.name} attachedToRef={evt.attachedToRef}>
      {if (evt.evtDef.isDefined) {
      evt.evtDef.get match {
        case evtDef: TimerEventDefinition => timerEventDefinition(evtDef)
        case evtDef: ErrorEventDefinition => errorEventDefinition(evtDef)
      }
    }}
    </boundaryEvent>
  }

  def timerEventDefinition(evtDef: TimerEventDefinition): Elem = {
    <timerEventDefinition>
      <timeDuration>
        {evtDef.timecondition}
      </timeDuration>
    </timerEventDefinition>
  }

  def errorEventDefinition(evtDef: ErrorEventDefinition): Elem = <errorEventDefinition errorRef={evtDef.errType}></errorEventDefinition>

  def startEvent(ev: StartEvent): Elem = <startEvent id={ev.id.toString} name={ev.name}></startEvent>

  def endEvent(ev: EndEvent): Elem = <endEvent id={ev.id.toString} name={ev.name}></endEvent>

  def parallelGateway(gg: SplitGateway): Elem = <parallelGateway id={gg.id.toString} name={gg.id.toString}></parallelGateway>

  def exclusiveGateway(gg: ExclusiveGateway): Elem = <exclusiveGateway id={gg.id.toString} name={gg.id.toString}></exclusiveGateway>

  /**
   * TEST
   *
   * @param args
   */
  def main(args: Array[String]): Unit = {
    val ll = conditionalExpr("").toString()

    print(ll)
  }

}
