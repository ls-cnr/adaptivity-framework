
function popupCredits(url, title, w, h) {
	var left = (screen.width/2)-(w/2);
	var top = (screen.height/2)-(h/2);
	return window.open(url, title, 'toolbar=no, location=no, directories=no, status=no, menubar=no, scrollbars=no, resizable=no, copyhistory=no, width='+w+', height='+h+', top='+top+', left='+left);
} 

function popupDialog() {
	$( "#dialog" ).dialog({
		width: 400
	});
}

function clickFile(){

	document.getElementById('my_file').click();
}

function readLocalFile(fileName, picFile){
	//console.log("FILE NAME-->"+fileName);
	$('#bpmn_image').attr('src',picFile);
	$('#bpmn_image').hide();
	jQuery.get(fileName, function(data) {
	    //console.log("XML-->"+data);
		callServicegetGoalsSpec(data);
	});
	$('#bpmn_image').load( function () {
        $('#bpmn_image').fadeIn('slow');
    });
}

function callServicegetGoalsSpec(xmlString){
	
	//"http://aose.pa.icar.cnr.it:8080/BPMN2Goal/GoalsFromBPMN"
	
	//var ipAdress="194.119.214.216:8080";
	//var ipAdress="localhost:8080";

	$('#bpmnTextArea').val(xmlString);

    console.log("FILE CONTENT-->"+xmlString);

	$.ajax({
		url : "./GoalsFromBPMN",
		//url : "http://"+ipAdress+"/BPMN2goal-1.0.0/GoalsFromBPMN",
		type: "POST",
		data:{"bpmnDiagramm":xmlString},
		crossDomain: true,
		dataType: 'json',
		success : function (data) {
			$('#goalsTextArea').val(data.goals);
			openTab(event, 'GOALSPEC');
			document.getElementById("goalSPecTab").className +=  " active";
		},
		error : function (richiesta,stato,errori) {
			alert("ERROR: servlet problems");
		}

	}); 
}
function uploadXMIFile(){

	//$('#customfileupload').html( $('#my_file').val());
	$('#bpmn_image').attr('src',"");
	$('#bpmn_image').hide();
	
	var file_input=document.getElementById("my_file").files[0];
	var fileReader = new FileReader();

	var xmlString =""

		fileReader.onload = function(fileLoadedEvent) 
		{
		xmlString = fileLoadedEvent.target.result;
		callServicegetGoalsSpec(xmlString);

		}
	fileReader.readAsText(file_input, "UTF-8");

}


function openTab(evt, tabName) {
	var i, tabcontent, tablinks;
	tabcontent = document.getElementsByClassName("tabcontent");
	for (i = 0; i < tabcontent.length; i++) {
		tabcontent[i].style.display = "none";
	}
	tablinks = document.getElementsByClassName("tablinks");
	for (i = 0; i < tablinks.length; i++) {
		tablinks[i].className = tablinks[i].className.replace(" active", "");
	}
	document.getElementById(tabName).style.display = "block";

	evt.currentTarget.className += " active";
}