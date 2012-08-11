
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

function hiddenField(name, value){
    return $("<input/>",
	     {type:"hidden",
	      name:name,
	      value:value});
}

function addButton(){
    var btn = $("<div/>", {class:"btn"}).append($("<i/>", {class:"icon-plus"}));
    return btn;
}

function infoElement(target, name, json){
    this.elem = $(target);
    this.name = name
    this.json = json;
    this.data = parseJsonList(json);

    // Add hidden field
    this.elem.append(hiddenField(name, json));


    // Add the button
    this.btn = addButton();
    this.btn.click($.proxy(function(){
	this.addElement("");
    }, this));
    this.elem.append(this.btn);

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
	   class:this.name,
	   value:v, 
	   placeholder:"Group Name"}));

    container.append(
	$("<span/>", 
	  {style:"cursor:pointer"}
	 ).append($("<i/>", {class:"icon-remove"})).click(
		function (){
		    container.remove();
		}));

    this.btn.before(container);
}

infoElement.prototype.serialize = function() {
    this.data = serializeElem("." + this.name);
    return this.data;
}



function slotElement(target, name, json){
    this.elem = $(target);
    this.name = name;
    this.json = json;
    this.data = parseJsonList(json);

    // Add hidden field
    this.elem.append(
	$("<input/>",
	  {type:"hidden",
	   name:name,
	   value:this.json}));
    
    // Add button
    this.btn = addButton();
    this.btn.click($.proxy(function(){
	this.addElement("");
    }, this));
    this.elem.append(this.btn);
    

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
	   class:"slot_name",
	   value:name,
	   placeholder:"5/31 (Thr) 5 ~ 5:30 pm"}));

    container.append(" with ");

    container.append(
	$("<input/>",
	  {type:"text",
	   class:"slot_limit",
	   value:limit,
	   placeholder:"1"}));

    container.append(" slots");


    container.append(
	$("<span/>", 
	  {style:"cursor:pointer"}
	 ).append($("<i/>", {class:"icon-remove"})).click(
	     function (){
		 container.remove();
	     }));

    this.btn.before(container);

}

slotElement.prototype.serialize = function() {
    var names = serializeElem(".slot_name");
    var limits = serializeElem(".slot_limit");
    
    slot = new Array();
    for(i=0;i<names.length;i++){
	slot[i] = new Array();
	slot[i][0] = names[i];
	slot[i][1] = parseInt(limits[i]);
    }
    this.data = slot;
    return slot;
};

var ie;
$(document).ready(function(){
    ie = new infoElement("#info", "info", "[\"heehong\", \"eunbee\"]");
    se = new slotElement("#slot", "slot", "[[\"test\", 1]]");
});