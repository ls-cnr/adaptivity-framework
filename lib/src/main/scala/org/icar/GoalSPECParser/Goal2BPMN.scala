package org.icar.GoalSPECParser

import org.icar.bpmn2goal._

import scala.xml.Elem

/**
 * Use this class to transform a [[Workflow]] into a BPMN representation that can be injected into Flowable.
 *
 * '''NOTE''': the graphical information about items are not inserted. This causes software like Camunda modeler to show
 * an error "No diagram to display". To address this issue, open the diagram with Eclipse (with Flowable plugin), modify
 * the position of some elements in the graphical editor, the graphical information are added automatically. Now the
 * diagram can be opened in Camunda.
 */
object Goal2BPMN {

  def getBPMN(process: Workflow, processName: String, processID: String): Elem = {
    <definitions xmlns:flowable="http://flowable.org/bpmn" xmlns="http://www.omg.org/spec/BPMN/20100524/MODEL"
                 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"
                 xmlns:activiti="http://activiti.org/bpmn" xmlns:bpmndi="http://www.omg.org/spec/BPMN/20100524/DI"
                 xmlns:omgdc="http://www.omg.org/spec/DD/20100524/DC" xmlns:omgdi="http://www.omg.org/spec/DD/20100524/DI"
                 typeLanguage="http://www.w3.org/2001/XMLSchema" expressionLanguage="http://www.w3.org/1999/XPath"
                 targetNamespace="http://www.activiti.org/test">
      <process id={processID} name={processName} isExecutable="true">
        {process.items.map(writeItem)}{process.flows.map(writeFlow)}
      </process>
    </definitions>
  }

  /*Service task------------------------------------------*/
  def serviceTask(task: ServiceTask): Elem = {
    <serviceTask id={task.id} name={task.label} flowable:class={scala.xml.Unparsed(task.className)}>
      <extensionElements>
        {if (task.extElems.isDefined) {
        task.extElems.get.listeners.map(executionListener)
        //        extensionElements(task.extElems.get)
      }}
      </extensionElements>
    </serviceTask>
  }

  def extensionElements(elems: FlowableExtentionElements): Elem = {
    <extensionElements>
      {elems.listeners.foreach(executionListener)}
    </extensionElements>
  }

  def gateway(gt: Gateway): Elem = gt match {
    case gt if GatewayType.withName(gt.gwtype) == GatewayType.Join =>

      <parallelGateway id={gt.id}></parallelGateway>

    case gt if GatewayType.withName(gt.gwtype) == GatewayType.Split =>

      <parallelGateway id={gt.id}></parallelGateway>

    case gt if GatewayType.withName(gt.gwtype) == GatewayType.Exclusive =>

      <exclusiveGateway id={gt.id}></exclusiveGateway>

  }

  def executionListener(listener: FlowableExecutionListener): Elem =
    <flowable:executionListener event={listener.event} class={scala.xml.Unparsed(listener.className)}></flowable:executionListener>

  def userTask(task: Task): Elem = <userTask id={task.id} name={task.label} flowable:candidateGroups=""></userTask>

  def writeItem(item: Item): Elem = item match {
    case t: Task => userTask(t)
    case st: ServiceTask => serviceTask(st)
    case gt: Gateway => gateway(gt)
    case ev: Event => event(ev)
  }

  def event(ev: Event): Elem = ev match {
    // ~~~~~ START EVENT ~~~~~
    case ev if EventType.withName(ev.eventtype) == EventType.Start =>
      <startEvent id={ev.id} name={ev.label}></startEvent>
    // ~~~~~ END EVENT ~~~~~
    case ev if EventType.withName(ev.eventtype) == EventType.End =>
      <endEvent id={ev.id} name={ev.label}></endEvent>
    // ~~~~~ BOUNDARY EVENT ~~~~~
    case ev if EventType.withName(ev.eventtype) == EventType.Boundary =>
      <boundaryEvent id={ev.id} name={ev.label} attachedToRef={getBoundaryEventAttachedRef(ev.definition)}>
        {if (ev.definition != null) {
        boundaryEventDefinition(ev.definition)
      }}
      </boundaryEvent>
  }

  def getBoundaryEventAttachedRef(evDef: EventDefinition): String = evDef match {
    case theDef: FlowableErrorEventDefinition => theDef.attachedToRef
    case theDef: FlowableTimerEventDefinition => theDef.attachedToRef
    case _ => ""
  }

  def boundaryEventDefinition(evtDef: EventDefinition): Elem = evtDef match {
    case t: FlowableErrorEventDefinition => <errorEventDefinition errorRef={t.errType}></errorEventDefinition>
    case t: FlowableTimerEventDefinition =>
      <timerEventDefinition>
        <timeDuration>
          {scala.xml.Unparsed(t.timecondition)}
        </timeDuration>
      </timerEventDefinition>
  }

  def writeFlow(s: Flow): Elem = s match {
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

}
