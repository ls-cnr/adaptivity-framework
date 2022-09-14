package org.icar.grounding

import org.icar.bpmn2goal.{EventType, FlowableExecutionListener, FlowableExtentionElements, ServiceTask}

/**
 * A concrete capability specifies the realization of a service described by an abstract capability in MUSA
 *
 * @param id                  an integer identifying this concrete capability. When multiple concrete capabilities are
 *                            mapped to the same service into one workflow, that is, we have multiple concrete
 *                            capabilities realizing the same service, it is necessary to discriminate capabilities.
 *                            This can be done through the id.
 * @param serviceName         the name of the service this capability realizes
 * @param className           the full path of the class that realizes the service. Note: the class must implement
 *                            the interface [[org.flowable.engine.delegate.JavaDelegate]]
 * @param startEventClassName the full path of the class to be executed when the corresponding service task begins its
 *                            execution. Note: the class must implement the interface
 *                            [[org.flowable.engine.delegate.ExecutionListener]]
 * @param endEventClassName   the full path of the class to be executed when the corresponding service task ends its
 *                            execution. Note: the class must implement the interface
 *                            [[org.flowable.engine.delegate.ExecutionListener]]
 * @author Davide Guastella
 */
case class ConcreteCapability(id: Int, // the ID of this concrete capability.
                              serviceName: String, //the id of the abstract capability
                              className: String,
                              startEventClassName: Option[String] = None,
                              endEventClassName: Option[String] = None) {

  def withID(theID: Int): ConcreteCapability = ConcreteCapability(theID, serviceName, className, startEventClassName, endEventClassName)

  /**
   * Convert this capability to a [[ServiceTask]]
   *
   * @return
   */
  def toServiceTask(): ServiceTask = {
    var listeners = List[FlowableExecutionListener]()
    startEventClassName match {
      case Some(s) => listeners = listeners ++ List(FlowableExecutionListener(EventType.Start.toString.toLowerCase(), s))
      case None =>
    }
    endEventClassName match {
      case Some(s) => listeners = listeners ++ List(FlowableExecutionListener(EventType.End.toString.toLowerCase(), s))
      case None =>
    }
    //ServiceTask(s"${serviceName}_${id}", serviceName, className, Some(FlowableExtentionElements(listeners)))
    ServiceTask(s"st_${id}", serviceName, className, Some(FlowableExtentionElements(listeners)))
  }

}

