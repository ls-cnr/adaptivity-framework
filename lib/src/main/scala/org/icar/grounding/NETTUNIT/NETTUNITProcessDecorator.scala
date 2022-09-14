package org.icar.grounding.NETTUNIT

import org.icar.grounding.NETTUNIT.ProcessDecorator.FlowableBoundaryErrorEventDecorator
import org.icar.grounding.processDecorator.ProcessDecoratorStrategy

object NETTUNITProcessDecorator extends ProcessDecoratorStrategy {
  addDecorator(new FlowableBoundaryErrorEventDecorator)
}

