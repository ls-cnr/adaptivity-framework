// CArtAgO artifact code for project musaOrg

package tools;

import cartago.*;
import org.icar.symbolic.GroundPredicate;
import org.icar.symbolic.StateOfWorld;

import java.util.ArrayList;
import java.util.List;

public class CurrentWorldState extends Artifact {

	private StateOfWorld state;

	void init() {
		List<GroundPredicate> args = new ArrayList<GroundPredicate>();
		state = new StateOfWorld(null);
	}

	@OPERATION
	void inc() {
		ObsProperty prop = getObsProperty("count");
		prop.updateValue(prop.intValue()+1);
		signal("tick");
	}
}

