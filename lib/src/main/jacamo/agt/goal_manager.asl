{ include("$jacamoJar/templates/common-cartago.asl") }
{ include("$jacamoJar/templates/common-moise.asl") }
{ include("$moiseJar/asl/org-obedient.asl") }

/* application domain goals */


//+!wsecs <- .print("writing sections...").
//+!wrefs <- .print("organising bibliography...").

//mission5
+!check_goal_validation <- .print("controllo validazione goal").

//+!pick_solution <- .print("pick abstratc solutions").
//+!ground_solution <- .print("find concrete solutions").
//+!form_group_validator <-  .print("form group validators").
//+!order_solution <- .print("ordino le soluzioni").
