///////////////////////////////////////////////////////////////////////////
// Copyright © 2014 Esri. All Rights Reserved.
// Modified October 2016 by Dave Mechenich
///////////////////////////////////////////////////////////////////////////

define([
    'jimu/BaseWidget',
    'dojo/_base/declare',
    'dojo/_base/lang',
    'dojo/_base/array',
    'dojo/_base/html',
    'dojo/dom',
    'dojo/on',
    'dojo/query',
    'dijit/registry',
    './LayerListView',
    './NlsStrings',
    'jimu/LayerInfos/LayerInfos',
	'jimu/PanelManager',
	'esri/geometry/scaleUtils'
  ],
  function(BaseWidget, declare, lang, array, html, dom, on,
  query, registry, LayerListView, NlsStrings, LayerInfos, PanelManager, scaleUtils) {

    var clazz = declare([BaseWidget], {
      //these two properties is defined in the BaseWiget
      baseClass: 'jimu-widget-layerList',
      name: 'layerList',
      _denyLayerInfosReorderResponseOneTime: null,
      //layerListView: Object{}
      //  A module is responsible for show layers list
      layerListView: null,
      operLayerInfos: null,
	  //***following vars for water qual parm control
	  unitRef: 0,
	  unitButtonID: "u0",
	  paraRef: 11,
	  paraButtonID: "p11",
	  //paraID: null,
	  //pLayerID: null,
	  curButtonID: null,
	  //curButton: null,
	  //unitParameter: null,
	  //currentLayer: null,
	  baseLayers: null,
	  chemLayers: null,
	  pwLayerID: 17,
	  layerLimits: null,
	  boundaryDisplayed: false,
	  wsColor: null,
	  
      startup: function() {
        this.inherited(arguments);
        NlsStrings.value = this.nls;
		this.layerLimits = this.config.layerChangeLOD.split(",").map(Number);
		this.baseLayers = this.config.baseLayerCnt;
		this.chemLayers = this.config.chemLayerCnt;
		this.wsColor = this.config.watShedColor;
        this._denyLayerInfosReorderResponseOneTime = false;
        // summary:
        //    this function will be called when widget is started.
        // description:
        //    according to webmap or basemap to create LayerInfos instance
        //    and initialize operLayerInfos;
        //    show layers list;
        //    bind events for layerLis;
        //if (this.map.itemId) {
          LayerInfos.getInstance(this.map, this.map.itemInfo)
            .then(lang.hitch(this, function(operLayerInfos) {
              this.operLayerInfos = operLayerInfos;
			  this._startLayers();
              this.showLayers();
              this.bindEvents();
              dom.setSelectable(this.layersSection, false);
            }));
        //} else {
        //  var itemInfo = this._obtainMapLayers();
        //  LayerInfos.getInstance(this.map, itemInfo)
        //    .then(lang.hitch(this, function(operLayerInfos) {
        //      this.operLayerInfos = operLayerInfos;
        //      this.showLayers();
        //      this.bindEvents();
        //      dom.setSelectable(this.layersSection, false);
        //    }));
        //};
		
		//var scaleRadio = query('input', this.printWidgetMapScale.domNode)[0];
        //utils.combineRadioCheckBoxWithLabel(scaleRadio, this.printWidgetMapScaleLabel);
		
		//**added to adj color of watersheds
		this.setWatershedColor();
      },
	  
	  _startLayers: function() {
		//**this added to change load dynamics to hopefully prevent odd partial erase of start layers
		//var psStartLayer = this.operLayerInfos.getLayerInfoById('Pri_Wells_702').setTopLayerVisible(true);
		var pwOperLayers = this.operLayerInfos.getLayerInfoArray();
		var pwStartLayer = pwOperLayers[this.pwLayerID];
		pwStartLayer.setTopLayerVisible(true);
		pwStartLayer = pwOperLayers[0];
		pwStartLayer.setTopLayerVisible(true);
	  },
	  
	  setWatershedColor: function() {
		var pwOperLayers = this.operLayerInfos.getLayerInfoArray();
		array.forEach(pwOperLayers, function(pwOperLayer) {
			if (pwOperLayer.title === 'Pri Wells - Watersheds'){
			    var layObj = pwOperLayer.getLayerObject().then(lang.hitch(this, function(layObj) {
					for (c = 0; c < 3; c++) {
						layObj.renderer.infos[c].symbol.color.setColor(this.wsColor);
					}
				}));
			}
        }, this);
      },
	  
	  onReceiveData: function(name, widgetId, data) {
		if(name == 'eSearch' && data.message != 'fromSearch'){
			//alert("eSearch");
            this.curButtonID = data.message;
		    this.upProcess();
		}
		//alert(name);
		//alert(data.message);
      },

	  _onUPClicked: function(event) {
		  //***parm button control
		  //this cross browser code is not checked
		  this.curButtonID = event.target.id || event.scrElement.id;
		  this.upProcess();
	  },
	  
	  upProcess: function() {
		  //***parm button handler
		  var Unit_Para = this.curButtonID.slice(0,1);
		  var upNum = this.curButtonID.slice(1);
		  var curButton = document.getElementById(this.curButtonID).className = "puButtonSel";
		  var pwLayerList = this.operLayerInfos.getLayerInfoArray();
		  var pwOpLayer = pwLayerList[this.pwLayerID].setTopLayerVisible(false);
		  if (Unit_Para == "u" && this.curButtonID != this.unitButtonID) {
            var oldButton = document.getElementById(this.unitButtonID).className = "puButton";
			this.unitButtonID = this.curButtonID;
			this.unitRef = upNum;
			var myLOD = this.map.getLevel(); 
		    switch (this.unitButtonID) {
			  case 'u0':
			    if (myLOD>this.layerLimits[0]) {
				  this.map.setLevel(this.layerLimits[0]);
			    }
			    break;
			  case 'u1':
			    if (myLOD>this.layerLimits[1]) {
				  this.map.setLevel(this.layerLimits[1]);
			    }
			    break;
			  case 'u2':
			    if (myLOD<this.layerLimits[2]) {
				  this.map.setLevel(this.layerLimits[2]);
			    }
		    }
          } else if (Unit_Para == "p" && this.curButtonID != this.paraButtonID) {
            var oldButton = document.getElementById(this.paraButtonID).className = "puButton";
			this.paraButtonID = this.curButtonID;
			this.paraRef = upNum;
          }
		  this.pwLayerID = ((Number(this.unitRef))*this.chemLayers) + Number(this.paraRef) + this.baseLayers;
		  this.publishData({
            message: this.pwLayerID
          }, false);
		  pwOpLayer = pwLayerList[this.pwLayerID].setTopLayerVisible(true);
	  },
	  
      destroy: function() {
        this._clearLayers();
        this.inherited(arguments);
      },

	  _baseIconClicked: function(event) {
		var pm = PanelManager.getInstance();
		var bmStatus = pm.getPanelById('_80_panel');
		if (typeof bmStatus === "object") {
			if (bmStatus.state == "opened") {
				pm.closePanel('_80_panel');
			}else{
				var bConfig = this.getBaseWidgetConfig();
		        pm.showPanel(bConfig[0]);
			}
		}else{
			var bConfig = this.getBaseWidgetConfig();
		    pm.showPanel(bConfig[0]);
		}
	  },	  
	  
	  _boundaryIconClicked: function(event) {
		  if (this.boundaryDisplayed){
			  this.boundaryDisplayed = false;
			  html.setStyle(this.layersSection, 'display', 'none');
		  }else{
			  this.boundaryDisplayed = true
			  html.setStyle(this.layersSection, 'display', '');
		  }
	  },	 
	  
	  getBaseWidgetConfig: function() {
        var ret = [];
        array.forEach(this.appConfig.widgetOnScreen.widgets, function(w){
            if(w.id == "_80") {
                ret.push(w);
            }
        }, this);
        return ret;
      },
	  
      /*_obtainMapLayers: function() {
		//***this is a web map bassed app
		// summary:
        //    obtain basemap layers and operational layers if the map is not webmap.
        var basemapLayers = [],
          operLayers = [];
        // emulate a webmapItemInfo.
        var retObj = {
          itemData: {
            baseMap: {
              baseMapLayers: []
            },
            operationalLayers: []
          }
        };
        array.forEach(this.map.graphicsLayerIds, function(layerId) {
          var layer = this.map.getLayer(layerId);
          if (layer.isOperationalLayer) {
            operLayers.push({
              layerObject: layer,
              title: layer.label || layer.title || layer.name || layer.id || " ",
              id: layer.id || " "
            });
          }
        }, this);
        array.forEach(this.map.layerIds, function(layerId) {
          var layer = this.map.getLayer(layerId);
          if (layer.isOperationalLayer) {
            operLayers.push({
              layerObject: layer,
              title: layer.label || layer.title || layer.name || layer.id || " ",
              id: layer.id || " "
            });
          } else {
            basemapLayers.push({
              layerObject: layer,
              id: layer.id || " "
            });
          }
        }, this);

        retObj.itemData.baseMap.baseMapLayers = basemapLayers;
        retObj.itemData.operationalLayers = operLayers;
        return retObj;
      },*/

      showLayers: function() {
        // summary:
        //    create a LayerListView module used to draw layers list in browser.
        this.layerListView = new LayerListView({
          operLayerInfos: this.operLayerInfos,
          layerListWidget: this,
          config: this.config
        }).placeAt(this.layerListBody);
      },

      _clearLayers: function() {
        // summary:
        //   clear layer list
        //domConstruct.empty(this.layerListTable);
        if (this.layerListView && this.layerListView.destroyRecursive) {
          this.layerListView.destroyRecursive();
        }
      },

      _refresh: function() {
        this._clearLayers();
        this.showLayers();
      },

      /****************
       * Event
       ***************/
      bindEvents: function() {
        // summary:
        //    bind events are listened by this module
        this.own(on(this.operLayerInfos,
          'layerInfosChanged',
          lang.hitch(this, this._onLayerInfosChanged)));

        this.own(on(this.operLayerInfos,
          'tableInfosChanged',
          lang.hitch(this, this._onLayerInfosChanged)));

        this.own(this.operLayerInfos.on('layerInfosIsVisibleChanged',
          lang.hitch(this, this._onLayerInfosIsVisibleChanged)));

        this.own(on(this.operLayerInfos,
          'updated',
          lang.hitch(this, this._onLayerInfosObjUpdated)));

        this.own(on(this.operLayerInfos,
          'layerInfosReorder',
          lang.hitch(this, this._onLayerInfosReorder)));

        this.own(on(this.map,
          'zoom-end',
          lang.hitch(this, this._onZoomEnd)));

        this.own(on(this.operLayerInfos,
          'layerInfosRendererChanged',
          lang.hitch(this, this._onLayerInfosRendererChanged)));
      },

      _onLayerInfosChanged: function(/*layerInfo, changedType*/) {
        this._refresh();
      },

      _onLayerInfosIsVisibleChanged: function(changedLayerInfos) {
        array.forEach(changedLayerInfos, function(layerInfo) {
          query("[class~='visible-checkbox-" + layerInfo.id + "']", this.domNode)
          .forEach(function(visibleCheckBoxDomNode) {
            var visibleCheckBox = registry.byNode(visibleCheckBoxDomNode);
            if(layerInfo.isVisible()) {
              visibleCheckBox.check();
            } else {
              visibleCheckBox.uncheck();
            }
          }, this);

        }, this);
      },

      _onLayerInfosObjUpdated: function() {
        this._refresh();
      },

      _onZoomEnd: function() {
        this.operLayerInfos.traversal(lang.hitch(this, function(layerInfo) {
          query("[class~='layer-title-div-" + layerInfo.id + "']", this.domNode)
          .forEach(function(layerTitleDivIdDomNode) {
            try {
              if (layerInfo.isInScale()) {
                html.removeClass(layerTitleDivIdDomNode, 'grayed-title');
              } else {
                html.addClass(layerTitleDivIdDomNode, 'grayed-title');
              }
            } catch (err) {
              console.warn(err.message);
            }
          }, this);
        }));
		//***code to check display unit
		var myLOD = this.map.getLevel(); 
		switch (this.unitButtonID) {
			case 'u0':
			    if (myLOD>this.layerLimits[0]) {
					if(myLOD>this.layerLimits[1]){
						this.curButtonID = 'u2';
					}else{
						this.curButtonID = 'u1';
					}
				    this.upProcess();
			    }
			    break;
			case 'u1':
			    if (myLOD>this.layerLimits[1]) {
				    this.curButtonID = 'u2';
				    this.upProcess();
			    }
			    break;
			case 'u2':
			    if (myLOD<this.layerLimits[2]) {
				    this.curButtonID = 'u1';
				    this.upProcess();
			    }
		}
      },

      _onLayerInfosReorder: function() {
        if(this._denyLayerInfosReorderResponseOneTime) {
          // denies one time
          this._denyLayerInfosReorderResponseOneTime = false;
        } else {
          this._refresh();
        }
      },

      _onLayerInfosRendererChanged: function(changedLayerInfos) {
        try {
          array.forEach(changedLayerInfos, function(layerInfo) {
            this.layerListView.redrawLegends(layerInfo);
          }, this);
        } catch (err) {
          this._refresh();
        }
      },

      onAppConfigChanged: function(appConfig, reason, changedData){
        /*jshint unused: false*/
        this.appConfig = appConfig;
      }

    });

    return clazz;
  });
