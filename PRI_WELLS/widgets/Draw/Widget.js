///////////////////////////////////////////////////////////////////////////
// Custom Color Ramp widget using the Draw icon and widget name
///////////////////////////////////////////////////////////////////////////

define([
    'dojo/_base/declare',
    'jimu/BaseWidget',
	'dojo/_base/lang',
	'dojo/_base/array',
	'jimu/LayerInfos/LayerInfos',
	'jimu/LayerInfos/LayerInfo',
	'jimu/PanelManager',
	'esri/renderers/ClassBreaksRenderer',
	'esri/Color',
	'esri/layers/FeatureLayer',
	'esri/layers/ArcGISDynamicMapServiceLayer',
	'esri/layers/LayerDrawingOptions',
	'esri/renderers/jsonUtils',
	'esri/map'
], function(declare, BaseWidget, lang, array, LayerInfos, LayerInfo, PanelManager, ClassBreaksRenderer, Color, FeatureLayer, ArcGISDynamicMapServiceLayer, LayerDrawingOptions, rendererJsonUtils, Map) {
  var clazz = declare([BaseWidget], {
    name: 'Draw',
    baseClass: 'jimu-widget-draw',
	operLayerInfos: null,
	baseLayers: null,
	chemLayers: null,
	rampA: null,
	rampB: null,
	rampC: null,
	rampOrder: null,
	
	startup: function() {
      //this.inherited(arguments);
      //NlsStrings.value = this.nls;
      LayerInfos.getInstance(this.map, this.map.itemInfo).then(lang.hitch(this, function(operLayerInfos) {
	    this.operLayerInfos = operLayerInfos;
      }));
	  this.baseLayers = this.config.baseLayerCnt;
	  this.chemLayers = this.config.chemLayerCnt;
	  //rampA: ['#0B2C7A','#20998F','#00DB00','#FFFF00','#EDA113','#C2523C'],
	  //rampB: ['#FED976','#FEB24C','#FD8D3C','#FC4E2A','#E31A1C','#B10026'],
	  //rampC: ['#8C510A','#BF812D','#DFB37D','#80CDC1','#35978F','#01665E'],
	  //rampOrder: ['std','std','std','std','std','std','std','std','std','std','std','std','std','rev','rev','std'],
	  this.rampA = this.config.rampA.split(",");
	  this.rampB = this.config.rampB.split(",");
	  this.rampC = this.config.rampC.split(",");
	  this.rampOrder = this.config.rampOrder.split(",");
	  this.paintRamps();
	  
	  var pm = PanelManager.getInstance();
	  if(window.innerWidth<600 || window.innerHeight<600){
	  	  pm.closePanel('_79_panel');
	  } 
    },
		  
	//onOpen: function() {
    //  this.paintRamps();
    //},
	
	paintRamps: function() {
		document.getElementById("rampA1").style.background = this.rampA[0];
		document.getElementById("rampA2").style.background = this.rampA[1];
		document.getElementById("rampA3").style.background = this.rampA[2];
		document.getElementById("rampA4").style.background = this.rampA[3];
		document.getElementById("rampA5").style.background = this.rampA[4];
		document.getElementById("rampA6").style.background = this.rampA[5];
		document.getElementById("rampB1").style.background = this.rampB[0];
		document.getElementById("rampB2").style.background = this.rampB[1];
		document.getElementById("rampB3").style.background = this.rampB[2];
		document.getElementById("rampB4").style.background = this.rampB[3];
		document.getElementById("rampB5").style.background = this.rampB[4];
		document.getElementById("rampB6").style.background = this.rampB[5];
		document.getElementById("rampC1").style.background = this.rampC[0];
		document.getElementById("rampC2").style.background = this.rampC[1];
		document.getElementById("rampC3").style.background = this.rampC[2];
		document.getElementById("rampC4").style.background = this.rampC[3];
		document.getElementById("rampC5").style.background = this.rampC[4];
		document.getElementById("rampC6").style.background = this.rampC[5];
	},

	_rampSelected: function() {
		var curRamp = [];
		if (document.getElementById("r1").checked) {
			curRamp = this.rampA;
		}
		if (document.getElementById("r2").checked) {
			curRamp = this.rampB;
		}
		if (document.getElementById("r3").checked) {
			curRamp = this.rampC;
		}
		
		var pwOperLayers = this.operLayerInfos.getLayerInfoArray();
		
		for (u = 0; u < 3; u++) {
		  for (p = 0; p < this.chemLayers; p++) {
		    pwOperLayer = pwOperLayers[this.baseLayers+(u*this.chemLayers)+p];
			var layObj = pwOperLayer.getLayerObject().then(lang.hitch(this, function(layObj) {
			  if (this.rampOrder[p] == 'std'){
			    for (c = 0; c < 6; c++) {
				  layObj.renderer.infos[c].symbol.color.setColor(curRamp[c]);
		        }
		      }else{
			    for (c = 0; c < 6; c++) {
			      layObj.renderer.infos[c].symbol.color.setColor(curRamp[5-c]);
		        }  
		      }
			  if (layObj.visible) {
			    layObj.hide();
				layObj.show();
			  }
			}));
		  }
		}
	},  
	
  });
  return clazz;
});