package tools

import cartago._
import org.icar.symbolic.StateOfWorld

class StateArtifact extends Artifact {

  def current = StateOfWorld(List.empty)

  @OPERATION
  def do_monitor : Unit = {
    println("artefatto in funzione")
  }

  @OPERATION
  def do_sense(function : String) : Unit = {
    println("artefatto in funzione: "+function)
  }

}