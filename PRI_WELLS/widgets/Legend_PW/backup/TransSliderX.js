/*
Copyright ©2014 Esri. All rights reserved.

TRADE SECRETS: ESRI PROPRIETARY AND CONFIDENTIAL
Unpublished material - all rights reserved under the
Copyright Laws of the United States and applicable international
laws, treaties, and conventions.

For additional information, contact:
Attn: Contracts and Legal Department
Environmental Systems Research Institute, Inc.
380 New York Street
Redlands, California, 92373
USA

email: contracts@esri.com
*/

define([
  'dijit/form/HorizontalSlider',
], function(HorizSlider) {

  var oSlider = function() {
    var sliderParams = {
	  minimum: 0,
	  maximum: 1,
	  intermediateChanges: true
	};
	alert(sliderParams.maximum)
	this.transHorizSlider = new HorizontalSlider(sliderParams, "sliderHere");
	this.transHorizSlider.startup();
  },

  return;
});
