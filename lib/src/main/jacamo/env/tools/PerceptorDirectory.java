// CArtAgO artifact code for project musaOrg

package tools;
import java.util.ArrayList;
import java.util.List;
import cartago.*;

public class PerceptorDirectory extends Artifact {

	private ArrayList<String> agents;
	private ArrayList<String> sensors;


	void init() {
		 agents = new ArrayList<>();
		 sensors = new ArrayList<>();

	}

 	@OPERATION
	void register(String agentName, String sensorName) {
		//String agente  = new String(name);
		//String sensore  = new String(name);
		agents.add(agentName);
		sensors.add(sensorName);
		System.out.println("registered " + agentName + " " + sensorName);
		signal("agent_check", agentName, sensorName);
	}


	@OPERATION
	void find_agent(String agentName) {
		for (int i = 0; i < agents.size(); i++) {
				String element = agents.get(i);
				if (agentName.compareTo(element) > 0) {
					System.out.println(agentName + "in lista");
				}
		}
	}

}
