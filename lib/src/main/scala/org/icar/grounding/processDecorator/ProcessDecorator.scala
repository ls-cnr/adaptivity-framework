package org.icar.grounding.processDecorator

import org.icar.bpmn2goal.Workflow

/**
 * A process decorator takes a concrete workflow as input and decorates it with additional elements. For example, a
 * decorator can add elements to the workflow needed for execution using a WFM.
 *
 * @author Davide Guastella
 */
trait ProcessDecorator {
  def apply(wf: Workflow): Workflow
}
