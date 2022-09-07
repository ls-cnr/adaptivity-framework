package org.icar.grounding

import org.icar.bpmn2goal.{FlowableExecutionListener, FlowableExtentionElements, ServiceTask}

import scala.xml.Elem

/**
 *
 * @param serviceName
 * @param className
 * @param startEventClassName
 * @param endEventClassName
 */
case class ConcreteCapability(serviceName: String,
                              className: String,
                              startEventClassName: Option[String] = None,
                              endEventClassName: Option[String] = None) {

  /**
   * Convert this capability to a [[ServiceTask]] instance
   *
   * @return
   */
  def toServiceTask(): ServiceTask = {
    var listeners = List[FlowableExecutionListener]()
    val startEvent = startEventClassName match {
      case Some(s) => listeners = listeners ++ List(FlowableExecutionListener("start", s))
      case None =>
    }
    val endEvent = endEventClassName match {
      case Some(s) => listeners = listeners ++ List(FlowableExecutionListener("end", s))
      case None =>
    }
    ServiceTask(serviceName, serviceName, className, Some(FlowableExtentionElements(listeners)))
  }

  /**
   * Convert this concrete capability to a ServiceTask representation in XML. The output string
   * can be used into a BPMN definition that can be then injected into Flowable.
   *
   * @return the BPMN representation of this capability
   */
  def toXMLServiceTask(): Elem = {
    <serviceTask id={serviceName} name={serviceName} flowable:class={className}>
      {if (startEventClassName.isDefined || endEventClassName.isDefined) {
      extentionElements(startEventClassName, endEventClassName)
    }}
    </serviceTask>
  }

  private def extentionElements(startEventClassName: Option[String] = None, endEventClassName: Option[String] = None): Elem = {
    <extensionElements>
      {startEventClassName match {
      case Some(s) => <flowable:executionListener event="start" class={s}></flowable:executionListener>
      case None =>
    }}{endEventClassName match {
      case Some(s) => <flowable:executionListener event="end" class={s}></flowable:executionListener>
      case None =>
    }}
    </extensionElements>
  }

}


