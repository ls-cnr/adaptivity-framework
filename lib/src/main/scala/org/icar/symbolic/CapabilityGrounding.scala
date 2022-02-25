package org.icar.symbolic

/**
 * Given an abstract capability may have input terms,
 * a CapabilityGrounding specifies how the action is instantiated into the domain
 * @param capability reference to the abstract capability
 * @param grounding map structure that associate a constant term to all the input terms
 */
case class CapabilityGrounding(capability: AbstractCapability, grounding: Map[String, ConstantTerm]) {
	def unique_id: String = {
		var unique_id: String = capability.id

		if (grounding.nonEmpty) {
			unique_id += "("
			var first = true
			for (v <- grounding.keys)
				if (first) {
					unique_id += v + "=" + grounding(v)
					first = false
				} else {
					unique_id += "," + v + "=" + grounding(v)
				}

			unique_id += ")"
		}

		unique_id
	}
}
