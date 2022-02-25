{ include("$jacamoJar/templates/common-cartago.asl") }
{ include("$jacamoJar/templates/common-moise.asl") }
{ include("$moiseJar/asl/org-obedient.asl") }

/* application domain goals */


//mission6




+!sense_environment <-
  .my_name(AgName);
  .print("My name is: ", AgName);
  //.concat(AgName, "_bis", SensorName);
  //register(AgName);

  .print("sensing environment").
