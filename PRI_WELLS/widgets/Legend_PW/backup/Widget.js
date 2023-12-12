///////////////////////////////////////////////////////////////////////////
// Copyright © 2014 Esri. All Rights Reserved.
// Modified D Mechenich Oct 2016 for Well Viewer
///////////////////////////////////////////////////////////////////////////

define([
    'dojo/_base/declare',
    'dojo/_base/lang',
    'dojo/_base/html',
    'dojo/on',
    './Utils',
    'dijit/_WidgetsInTemplateMixin',
    'jimu/BaseWidget',
    'jimu/LayerInfos/LayerInfos',
    'esri/dijit/Legend'
], function(declare, lang, html, on, legendUtils,
_WidgetsInTemplateMixin, BaseWidget, LayerInfos, Legend) {

  var clazz = declare([BaseWidget, _WidgetsInTemplateMixin], {
    name: 'Legend',
    baseClass: 'jimu-widget-legend_pw',
    legend: null,
    _jimuLayerInfos: null,
	operLayerInfos: null,
	pwLayerID: 'PW_Viewer_117',
	pwLayer: null,
	HorizSlider: null,
	
    startup: function() {
      this.inherited(arguments);
	  LayerInfos.getInstance(this.map, this.map.itemInfo)
        .then(lang.hitch(this, function(operLayerInfos) {
           this.operLayerInfos = operLayerInfos;
		   this.pwLayer = operLayerInfos.getLayerInfoById(this.pwLayerID);
        }));
    },

    onOpen: function() {
      this._jimuLayerInfos = LayerInfos.getInstanceSync();
      var legendParams = {
        arrangement: this.config.legend.arrangement,
        autoUpdate: this.config.legend.autoUpdate,
        respectCurrentMapScale: this.config.legend.respectCurrentMapScale,
        //respectVisibility: false,
        map: this.map,
        layerInfos: this._getLayerInfosParam()
      };
      this.legend = new Legend(legendParams, html.create("div", {}, this.domNode));
      this.legend.startup();
      this._bindEvent();
      
	  require(["dijit/form/HorizontalSlider"], function(HorizontalSlider) {
        //create the slider
        var horzSlider = new HorizontalSlider({
            minimum: 0,
            maximum: 1,
            value: 0,
            intermediateChanges: true,
            style: "width: 160px",
			//onChange: _onSliderChange(value);
			onChange: function(value) {
            //  //this._onSliderChange(value);
			  dojo.byId("opval").innerHTML = 1-value;
			}
		}, "sliderHere");
		//start widgets
		horzSlider.startup();
      });
    },
	
    onClose: function() {
      this.legend.destroy();
    },
	
	_onSliderChange: function(svalue) {
	  alert("in sub");
	  this.pwLayer.setOpacity(1-svalue);
	},
	
    _bindEvent: function() {
      if(this.config.legend.autoUpdate) {
        this.own(on(this._jimuLayerInfos,
                    'layerInfosIsShowInMapChanged',
                    lang.hitch(this, 'refreshLegend')));

        this.own(on(this._jimuLayerInfos,
                    'layerInfosChanged',
                    lang.hitch(this, 'refreshLegend')));

        this.own(on(this._jimuLayerInfos,
                    'layerInfosRendererChanged',
                    lang.hitch(this, 'refreshLegend')));
      }
    },
	
    _getLayerInfosParam: function() {
      var layerInfosParam;
      if(this.config.legend.layerInfos === undefined) {
        // widget has not been configed.
        layerInfosParam = legendUtils.getLayerInfosParam();
      } else {
        // widget has been configed, respect config.
        layerInfosParam = legendUtils.getLayerInfosParamByConfig(this.config.legend);
      }

      // filter layerInfosParam
      //return this._filterLayerInfsParam(layerInfosParam);
      return layerInfosParam;
    },

    refreshLegend: function() {
      var layerInfos = this._getLayerInfosParam();
      this.legend.refresh(layerInfos);
    }
  });
  return clazz;
});
