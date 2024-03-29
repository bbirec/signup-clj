
function parseJsonList(json){
    if(json == ""){
	return [];
    }
    else{
	return JSON.parse(json);
    }
}

function serializeElem(target){
    var elems = $(target);
    var output = new Array();
    for(i=0;i<elems.length;i++){
	output.push(elems[i].value);
    }
    return output;
}

function addButton(){
    return $("<div/>", {"class":"btn"}).append($("<i/>", {"class":"icon-plus"})).append("Add");
}


function hiddenField(name, value){
    return $("<input/>",
	     {type:"hidden",
	      name:name,
	      value:value});
}


function infoElement(target, name, editable){
    this.elem = $(target);
    this.name = name
    this.json = this.elem.text();
    this.data = parseJsonList(this.json);
    this.editable = editable;

    // Clear data
    this.elem.text("");


    // Add hidden field
    this.hidden = hiddenField(name, this.json)
    this.elem.append(this.hidden);


    // Add the button
    if(editable){
	this.btn = addButton();
	this.btn.click($.proxy(function(){
	    this.addElement("");
	}, this));
	this.elem.append(this.btn);
    }

    // Add the data
    for(i=0;i<this.data.length;i++){
	this.addElement(this.data[i]);
    }
}

infoElement.prototype.addElement = function(v) {
    v = typeof v !== 'undefined' ? v : "";
    var container = $("<div/>");
    container.append(
	$("<input/>", 
	  {type:"text", 
	   "class":this.name,
	   value:v, 
	   placeholder:"Group Name"}));

    if(this.editable){
	container.append(
	$("<span/>", 
	  {style:"cursor:pointer",
	   "class":"btn"}
	 ).append($("<i/>", {"class":"icon-remove"})).append("Delete").click(
	     function (){
		    container.remove();
		}));
	this.btn.before(container);
    }
    else{
	this.elem.append(container);
    }

    
}

infoElement.prototype.serialize = function() {
    this.data = serializeElem("." + this.name);
    this.json = JSON.stringify(this.data);
    this.hidden.val(this.json);
    return this.data;
}



function slotElement(target, name, editable){
    this.elem = $(target);
    this.name = name;
    this.json = this.elem.text();
    this.data = parseJsonList(this.json);
    this.editable = editable;

    // Clear data
    this.elem.text("");


    // Add hidden field
    this.hidden = hiddenField(name, this.json);
    this.elem.append(this.hidden);
    
    // Add button
    if(editable){
	this.btn = addButton();
	this.btn.click($.proxy(function(){
	    this.addElement("", 1);
	}, this));
	this.elem.append(this.btn);
    }

    // Add element
    for(i=0;i<this.data.length;i++){
	this.addElement(this.data[i][0], this.data[i][1]);
    }
}

slotElement.prototype.addElement = function(name, limit){
    var container = $("<div/>");
    container.append(
	$("<input/>",
	  {type:"text",
	   "class":"slot_name",
	   value:name,
	   placeholder:"5/31 (Thr) 5 ~ 5:30 pm"}));

    container.append(" with ");

    container.append(
	$("<input/>",
	  {type:"text",
	   "class":"slot_limit",
	   value:limit,
	   placeholder:"1",
	   disabled:!this.editable}));

    container.append(" slots");

    if(this.editable){
	container.append(
	    $("<span/>", 
	      {style:"cursor:pointer",
	       "class":"btn"}
	     ).append($("<i/>", {"class":"icon-remove"})).append("Delete").click(
		 function (){
		     container.remove();
		 }));
	this.btn.before(container);
    }
    else{
	this.elem.append(container);
    }

}

slotElement.prototype.serialize = function() {
    var names = serializeElem(".slot_name");
    var limits = serializeElem(".slot_limit");
    
    var slot = new Array();
    for(i=0;i<names.length;i++){
	slot[i] = new Array();
	slot[i][0] = names[i];
	slot[i][1] = parseInt(limits[i]);
    }
    this.data = slot;
    this.json = JSON.stringify(this.data);
    this.hidden.val(this.json);

    return slot;
};


var ie;
var se;
$(document).ready(function(){
    ie = new infoElement("#info", "info", editable);
    se = new slotElement("#slot", "slot", editable);
    $("#signup-form").submit(function(){
	ie.serialize();
	se.serialize();
	return true;
    });
});