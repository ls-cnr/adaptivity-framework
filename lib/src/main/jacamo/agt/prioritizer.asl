{ include("$jacamoJar/templates/common-cartago.asl") }
{ include("$jacamoJar/templates/common-moise.asl") }
{ include("$moiseJar/asl/org-obedient.asl") }

/* application domain goals */



//mission4

+!pick_solution <- .print("pick abstratc solutions").
+!ground_solution <- .print("find concrete solutions").
+!form_validator_group <-  .print("form group validators").
+!order_solution <- .print("ordino le soluzioni").
