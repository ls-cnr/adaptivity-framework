package org.icar.bpmn2goal

import java.io.File; import javax.swing.JFileChooser;
import javax.swing.filechooser.FileSystemView;

import java.io.FileInputStream

object goal_extractor extends App {

  //val file = "process/email_voting_example.bpmn" //procurement_example val
  //file = "process/simplified_email_voting.bpmn" //procurement_example val file
  //= "resources/main/process/peer_review_process.bpmn"

  val jfc = new JFileChooser(FileSystemView.getFileSystemView.getHomeDirectory)

  var file = ""

  val returnValue = jfc.showOpenDialog(null)

  if (returnValue == JFileChooser.APPROVE_OPTION) {
    val selectedFile = jfc.getSelectedFile
    file = selectedFile.getAbsolutePath
  }

  if (file != "") {
    val is = new FileInputStream(file)
    val Parser = new bpmn_parser(is)
    val report = Parser.fullFromInputStream
    val initial = Parser.initial_state

    println(report)
    println(initial)
  }
}
