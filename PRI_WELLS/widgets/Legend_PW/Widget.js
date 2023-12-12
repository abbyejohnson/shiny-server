///////////////////////////////////////////////////////////////////////////
// Copyright © 2014 Esri. All Rights Reserved.
// Modified D Mechenich Oct 2016 for Well Viewer
///////////////////////////////////////////////////////////////////////////

define([
    'dojo/_base/declare',
    'dojo/_base/lang',
	'dojo/_base/array',
    'dojo/_base/html',
	'dojo/dom',
    'dojo/on',
    './Utils',
    'dijit/_WidgetsInTemplateMixin',
    'jimu/BaseWidget',
    'jimu/LayerInfos/LayerInfos',
    'esri/dijit/Legend',
	'dijit/form/HorizontalSlider',
	//'dijit/form/HorizontalRuleLabels',
	//'dojo/dom-class',
    //'dojo/dom-style'
	'jimu/PanelManager'
], function(declare, lang, array, html, dom, on, legendUtils,
_WidgetsInTemplateMixin, BaseWidget, LayerInfos, Legend, HorizontalSlider, PanelManager) {

  var clazz = declare([BaseWidget, _WidgetsInTemplateMixin], {
    name: 'Legend',
    baseClass: 'jimu-widget-legend_pw',
    legend: null,
    _jimuLayerInfos: null,
	operLayerInfos: null,
	pwLayer: null,
	horzSlider: null,
	
    startup: function() {
      this.inherited(arguments);
	  LayerInfos.getInstance(this.map, this.map.itemInfo)
        .then(lang.hitch(this, function(operLayerInfos) {
           this.operLayerInfos = operLayerInfos;
		   //set current layer to startup default of 16 for Nitrate by County
		   var pwLayerList = this.operLayerInfos.getLayerInfoArray();
		   this.pwLayer = pwLayerList[17];
        }));
	  this._createSlider();
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
	  //this._createSlider();
    },
	
	onReceiveData: function(name, widgetId, data, historyData) {
	  //alert("onreceive");
      if(name !== 'LayerList'){
        return;
      }
	  //alert(data.message);
	  var curOpacity = this.pwLayer.getOpacity();
	  //alert(curOpacity);
	  var pwLayerList = this.operLayerInfos.getLayerInfoArray();
	  //alert(pwLayerList.length);
	  this.pwLayer = pwLayerList[data.message];
	  this.pwLayer.setOpacity(curOpacity);
    },
	
    _createSlider: function () {
		//this.horzSliderLabels = new dijit.form.HorizontalRuleLabels({ 
        //    container: "topDecoration", 
        //    count: 2, 
        //    labels: ["Opaque","Transparent"], 
        //    style: "height:2em;font-size:12px;color:gray;" 
        //}, "sliderLabelsHere"); 
        this.horzSlider = new HorizontalSlider({
            minimum: 0,
            maximum: 1,
            value: 0,
            intermediateChanges: true,
            style: "width: 160px",
		}, "sliderHere");
		this.horzSlider.on("change", lang.hitch(this, this._sliderChange));
    },		

    _sliderChange: function (value) {
	  this.pwLayer.setOpacity(1-value);
    },	
	
	_rampsIconClicked: function(event) {
		var pm = PanelManager.getInstance();
		var bmStatus = pm.getPanelById('_81_panel');
		if (typeof bmStatus === "object") {
			if (bmStatus.state == "opened") {
				pm.closePanel('_81_panel');
			}else{
				var bConfig = this.getOnScreenWidgetConfig("_81");
		        pm.showPanel(bConfig[0]);
				//if(window.innerWidth<600 || window.innerHeight<600){
				//	pm.closePanel('_79_panel');
		        //}
			}
		}else{
			var bConfig = this.getOnScreenWidgetConfig("_81");
		    pm.showPanel(bConfig[0]);
			//if(window.innerWidth<600 || window.innerHeight<600){
			//	pm.closePanel('_79_panel');
		    //}
		}
	},	  

    _eSearchIconClicked: function(event) {
	    var pm = PanelManager.getInstance();
			var bConfig = this.getPoolWidgetConfig("widgets_eSearch_Widget_93");
		    pm.showPanel(bConfig[0]);
	},	  
	
	getOnScreenWidgetConfig: function(wID) {
        var ret = [];
        array.forEach(this.appConfig.widgetOnScreen.widgets, function(w){
            if(w.id == wID) {
                ret.push(w);
            }
        }, this);
        return ret;
    },
	
	getPoolWidgetConfig: function(wID) {
        var ret = [];
        array.forEach(this.appConfig.widgetPool.widgets, function(w){
            if(w.id == wID) {
                ret.push(w);
            }
        }, this);
        return ret;
    },
	
	
    onClose: function() {
      this.legend.destroy();
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
	  //alert("refresh legend triggered");
      var layerInfos = this._getLayerInfosParam();
      this.legend.refresh(layerInfos);
    }
  });
  return clazz;
});
