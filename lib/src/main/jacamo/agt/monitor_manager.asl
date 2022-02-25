{ include("$jacamoJar/templates/common-cartago.asl") }
{ include("$jacamoJar/templates/common-moise.asl") }
//{ include("$moiseJar/asl/org-obedient.asl") }

/* application domain goals */
// plans to handle obligations
// obligation to commit to a mission
+obligation(Ag,Norm,committed(Ag,Mission,Scheme),Deadline)[artifact_id(ArtId),workspace(_,_,W)]
    : .my_name(Ag)
   <- .print("I am obliged to commit to ",Mission," on ",Scheme,"... doing so");
      commitMission(Mission)[artifact_name(Scheme), wid(W)].

//obligation(monitor_manager,(enabled(s1,form_perceptor_group) & (not failed(s1,form_perceptor_group))),done(s1,form_perceptor_group,monitor_manager),1671635174509)
//obligation(monitor_manager,(enabled(s1,update_world) & (not failed(s1,update_world))),done(s1,update_world,monitor_manager),1671635774888)

// obligation to achieve a goal
+obligation(Ag,Norm,What,Deadline)[artifact_id(ArtId)]
    : .my_name(Ag) & (satisfied(Scheme,Goal)=What | done(Scheme,Goal,Ag)=What) & Goal=form_perceptor_group
   <- //.print(" ---> working to achieve ",Goal," in scheme ",Scheme);
      !Goal[scheme(Scheme)];
      .print(" <--- done form_perceptor_group");
      //resetGoal(Goal)[artifact_id(ArtId)];
			//.wait(5000);
			goalAchieved(Goal)[artifact_id(ArtId)];
			.

+obligation(Ag,Norm,What,Deadline)[artifact_id(ArtId)]
    : .my_name(Ag) & (satisfied(Scheme,Goal)=What | done(Scheme,Goal,Ag)=What) & Goal=update_world
   <- //.print(" ---> working to achieve ",Goal," in scheme ",Scheme);
      !Goal[scheme(Scheme)];
      .print(" <--- done update_word");
			-obligation(Ag,Norm,What,Deadline)[artifact_id(ArtId)];
			.wait(5000);
			resetGoal(Goal)[artifact_id(ArtId)];
			//goalAchieved(Goal)[artifact_id(ArtId)]
			.


// an unknown type of obligation was received
+obligation(Ag,Norm,What,DeadLine)
   : .my_name(Ag)
   <- .print("I am obliged to ",What,", but I don't know what to do!").

// drop intentions for obligations that are not active anymore
-obligation(Ag,_,What,_)
   :  .my_name(Ag) & (satisfied(Scheme,Goal)=What | done(Scheme,Goal,Ag)=What) &
      .intend(Goal)
   <- .print("I am not obliged to achieve ",Goal," for scheme ",Scheme," anymore, dropping the intention.");
      .drop_intention(Goal[scheme(Scheme)]).


//mission2

+!activate_perception <-

	.print("activating sensors ...");
	.

+!form_perceptor_group <-

 //lookup("perceptor_directory", ArtId);
 //.print(ArtId);

	register("perceptor1", "termometro");
	//register("perceptor2");
	.print("loading sensors ...").

+!update_world <-
	do_sense(my_predicate);
	.print("reading sensors ...").

+agent_check(S, M) <-
	.print("Agent check");
	.print("agente ", S, " sensore ", M).
