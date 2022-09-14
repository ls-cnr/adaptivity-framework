package org.icar.grounding.processDecorator

import org.icar.bpmn2goal.Workflow

import scala.collection.mutable.ListBuffer

/**
 * A decorator strategy defines the pipeline by which an ordered set of decorators are applied to a workflow. A
 * strategy accepts a set of [[ProcessDecorators]] that are (in order) applied to transform a concrete workflow.
 *
 * @author Davide Guastella
 */
abstract class ProcessDecoratorStrategy {
  private val decorators = new ListBuffer[ProcessDecorator]

  /**
   * Add a new process decorator. If other decorators are already present, then the added decorator will be executed at
   * after the execution of the previous ones.
   *
   * @param d
   */
  def addDecorator(d: ProcessDecorator): Unit = decorators.addOne(d)

  /**
   * Apply all decorators to the input workflow. The decorators are applied according to the order in which they
   * were added.
   *
   * @param wf the workflow to decorate
   * @return the decorated workflow
   */
  def apply(wf: Workflow): Workflow = {
    var theWf = wf.copy(wf.datatypes, wf.items, wf.flows, wf.data)
    for (decorator <- decorators) {
      theWf = decorator.apply(theWf)
    }
    theWf
  }

}
