package org.icar.GoalSPECParser

import org.icar.bpmn2goal._
import org.icar.symbolic.{SequenceFlow, _}

import scala.xml.Elem

/**
 * Use this class to transform a [[Workflow]] into a BPMN representation that can be injected into Flowable.
 */
object Goal2BPMN {

  def getBPMN(process: Workflow): Elem = {
    val processID = ""
    val processName = ""
    <definitions xmlns:flowable="http://flowable.org/bpmn" xmlns="http://www.omg.org/spec/BPMN/20100524/MODEL"
                 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"
                 xmlns:activiti="http://activiti.org/bpmn" xmlns:bpmndi="http://www.omg.org/spec/BPMN/20100524/DI"
                 xmlns:omgdc="http://www.omg.org/spec/DD/20100524/DC" xmlns:omgdi="http://www.omg.org/spec/DD/20100524/DI"
                 typeLanguage="http://www.w3.org/2001/XMLSchema" expressionLanguage="http://www.w3.org/1999/XPath"
                 targetNamespace="http://www.activiti.org/test">
      <process id={processID} name={processName} isExecutable="true">
        {process.items.map(writeItems)}{process.flows.map(writeFlows)}
      </process>
    </definitions>
  }

  /*Service task------------------------------------------*/
  def serviceTask(task: ServiceTask): Elem = {
    <serviceTask id={task.id} flowable:class={task.className}>
      {if (task.extElems.isDefined) {
      extentionElements(task.extElems.get)
    }}
    </serviceTask>
  }

  def gateway(gt: Gateway): Elem = gt match {
    case gt if GatewayType.withName(gt.gwtype) == GatewayType.Join =>

      <parallelGateway id={gt.id}></parallelGateway>

    case gt if GatewayType.withName(gt.gwtype) == GatewayType.Split =>

      <parallelGateway id={gt.id}></parallelGateway>

    case gt if GatewayType.withName(gt.gwtype) == GatewayType.Exclusive =>

      <exclusiveGateway id={gt.id}></exclusiveGateway>

  }

  def extentionElements(elems: FlowableExtentionElements): Elem = {
    <extensionElements>
      {elems.listeners.foreach(executionListener)}
    </extensionElements>
  }

  def executionListener(listener: FlowableExecutionListener): Elem =
    <flowable:executionListener event={listener.event} class={listener.className}></flowable:executionListener>

  def userTask(task: Task): Elem = <userTask id={task.id} name={task.label} flowable:candidateGroups=""></userTask>

  def writeItems(item: Item): Elem = item match {
    case t: Task => userTask(t)
    case st: ServiceTask => serviceTask(st)
    case gt: Gateway => gateway(gt)
    case ev: Event => event(ev)
  }

  def event(ev: Event): Elem = ev match {
    case ev if EventType.withName(ev.eventtype) == EventType.Start =>
      <startEvent id={ev.id}></startEvent>

    case ev if EventType.withName(ev.eventtype) == EventType.End =>
      <endEvent id={ev.id}></endEvent>

  }

  def writeFlows(s: Flow): Elem = s match {
    case sf: org.icar.bpmn2goal.SequenceFlow => writeSequenceFlows(sf)
    //TODO other flow types here
  }

  def writeSequenceFlows(s: org.icar.bpmn2goal.SequenceFlow): Elem = {
    val sourceRef = s.start match {
      case s if s != null => s.id
      case _ => ""
    }
    val targetRef = s.end match {
      case s if s != null => s.id
      case _ => ""
    }
    <sequenceFlow id={s.id} sourceRef={sourceRef} targetRef={targetRef}>
      {if (s.condition.isDefined) {
      conditionalExpr(s.condition.get.toString)
    }}
    </sequenceFlow>
  }

  def conditionalExpr(expression: String): Elem =
    <conditionExpression xsi:type="tFormalExpression">
      {scala.xml.Unparsed("<![CDATA[%s]]>".format(expression))}
    </conditionExpression>

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

}
