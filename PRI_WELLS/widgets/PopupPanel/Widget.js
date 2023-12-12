///////////////////////////////////////////////////////////////////////////
// Popup Panel Widget - modified by Dave Mechenich, ori framework from Robert Scheitlin's widget
///////////////////////////////////////////////////////////////////////////
/*global define*/
define([
  'dojo/_base/declare',
  'dijit/_WidgetsInTemplateMixin',
  'jimu/BaseWidget',
  'jimu/dijit/Message',
  'esri/domUtils',
  'esri/dijit/Popup',
  'esri/tasks/QueryTask',
  'esri/tasks/query',
  'dojo/on',
  'dojo/query',
  'dojo/_base/html',
  'dojo/dom-class',
  'dojo/dom-construct',
  'dojo/_base/lang',
  'jimu/WidgetManager',
  'jimu/PanelManager',
  'jimu/MapManager',
  'dojo/i18n!esri/nls/jsapi',
  'dojo/_base/array',
  'dijit/layout/ContentPane',
  'esri/InfoTemplate'
],
  function (
    declare,
    _WidgetsInTemplateMixin,
    BaseWidget,
    Message,
    domUtils,
    Popup,
	QueryTask,
	Query,
    on,
    query,
    html,
    domClass,
    domConstruct,
    lang,
    WidgetManager,
    PanelManager,
    MapManager,
    esriBundle,
    array,
	InfoTemplate
  ) {
    return declare([BaseWidget, _WidgetsInTemplateMixin], {

      baseClass: 'widget-popuppanel',
      name: 'PopupPanel',
      label: 'Popup Panel',
      popup: null,
      zt: null,
      pt: null,
	  pr: null,
      popupMenu: null,
      //featureActionManager: null,
      inPanel: null,
	  curUnitID: 0,
	  curLayerID: 11,
	  baseLayers: null,
	  chemLayers: null,
	  chemOffset: null,
	  //paraInfos: null,
	  paraRanges: null,
	  paraTitle: null,
	  paraLimits: null,
	  paraDec: null,
	  paraX: null,
	  dataSource: "P",
	  unitList: null,
	  xNum: null,
	  xLocation: null,
	  fSearch: false,
	  zomLOD: null,
	  lineBreak: 43,
	  preHeight: 298,

      postCreate: function () {
        this.inherited(arguments);
		this.baseLayers = this.config.baseLayerCnt;
		this.chemLayers = this.config.chemLayerCnt;
		this.chemOffset = this.config.chemLayerOff;
		this.zoomLOD = this.config.zoomLODs.split(",").map(Number);
        domUtils.hide(this.actionsPaneDiv);
        this.popup = this.map.infoWindow;
        this.zt = domConstruct.toDom('<a title="Zoom" to="" class="action zoomTo" href="javascript:void(0);"><span>' +
                                    esriBundle.widgets.popup.NLS_zoomTo + '</span></a>');
        domConstruct.place(this.zt, this.actionsListDiv);
        this.pt = domConstruct.toDom('<a title="Pan" to="" class="action panTo" href="javascript:void(0);"><span>' + this.nls.panto + '</span></a>');
        domConstruct.place(this.pt, this.actionsListDiv);
		this.pr = domConstruct.toDom('<a title="Print" to="" class="action Print" href="javascript:void(0);"><span>' + this.nls.print + '</span></a>');
        domConstruct.place(this.pr, this.actionsListDiv);
        
        this.onWindowResize();
		this.setEvtHandlers();
      },

      startup: function () {
		//**get the para info array from config.json
		//this.paraInfos = this.config.paras;
		this.paraTitle = this.config.paras[11].title;
		this.paraRanges = this.config.paras[11].ranges.split(",");
		this.paraLimits = this.config.paras[11].limits.split(",");
        this.paraDec = Math.pow(10, this.config.paras[11].dec);
		this.lineBreak = this.config.lineBreakTitle;
        //**show legend widget
		//the popup needs to start first; open legend here rather than autostart??
		var bConfig = this.getWidgetConfig("_79");
		PanelManager.getInstance().showPanel(bConfig[0]);
		
        this.inherited(arguments);
        this.inPanel = this.getPanel();
        //this.displayPopupContent(this.popup.getSelectedFeature());
		//close this popup widget
        if(this.config.closeAtStart){
        //  if(!this.popup.getSelectedFeature()){
        //    setTimeout(lang.hitch(this, function(){
              this.closeWidget();
        //    }), 300);
        //  }
        }
      },

	  onOpen: function () {
        var mapMan = MapManager.getInstance();
        if(mapMan.isMobileInfoWindow){
          this.map.setInfoWindow(mapMan._mapInfoWindow);
          mapMan.isMobileInfoWindow = false;
        }
        this.map.infoWindow.set("popupWindow", false);
      },
	  
      onWindowResize: function(){
        var mapMan = MapManager.getInstance();
        if(mapMan.isMobileInfoWindow){
          this.map.setInfoWindow(mapMan._mapInfoWindow);
          this.popup = this.map.infoWindow;
          mapMan.isMobileInfoWindow = false;
        }
		//this.setEvtHandlers();
      },

	  getWidgetConfig: function(wid) {
		//_79 is legend widget ID
        var ret = [];
        array.forEach(this.appConfig.widgetOnScreen.widgets, function(w){
            if(w.id == wid) {
                ret.push(w);
            }
        }, this);
        return ret;
      }, 
	  
	  closeWidget: function() {
		//**just set to close with PanelManager
        PanelManager.getInstance().closePanel(this.inPanel);
		//alert(this.inPanel);
		//Get widgetid by using this id
		//PanelManager.getInstance().closePanel(widgetid + ‘_panel’);
		//PanelManager.getInstance().closePanel(this.id + ‘_panel’);
		
        //var pm = PanelManager.getInstance().getPanelById(this.id + '_panel');
      },
	  
	  setEvtHandlers: function(){
        //this.own(on(this.popup, "selection-change", lang.hitch(this, function () {
          //this.selectedFeature = evt.target.getSelectedFeature();
        //  this.displayPopupContent(this.popup.getSelectedFeature());
        //})));

        /*this.own(on(this.popup, "clear-features", lang.hitch(this, function (evt) {
        //  if(this.instructions){
        //    domUtils.show(this.instructions);
            //this.instructions.innerHTML = this.nls.selectfeatures;
        //  }
		
          if(this.popupContent){
            this.popupContent.set("content", "");
          }
          domUtils.hide(this.pager);
          domUtils.show(this.instructions);
          domUtils.hide(this.actionsPaneDiv);
		  if (this.dataSource != 'Q'){
			  this.unitList = null;
		  }
		  if (this.fSearch){
			  this.fSearch = false;
			  this.publishData({
                  message: "clearSearch"
		      });
		  }
        })));*/

        this.own(on(this.popup, "set-features", lang.hitch(this, function(){
          if(!this.popup.features){
			//this should not be possible here alert("no features");
            domUtils.hide(this.pager);
            domUtils.show(this.instructions);
            domUtils.hide(this.actionsPaneDiv);
            return;
          }
          if(this.popup.features.length === 0){
			//this should not be possible here alert("0 features");
            domUtils.show(this.instructions);
            domUtils.hide(this.actionsPaneDiv);
          }else{
            domUtils.hide(this.instructions);
            domUtils.show(this.actionsPaneDiv);
			if(this.popup.features[0].attributes.sS){
				this.fSearch = true;
				this.dataSource = this.popup.features[0].attributes.sS;
				this.unitList = null;
			}else{
                if (this.fSearch){
			        this.publishData({
                        message: "clearSearch"
		            });
					this.fSearch = false;
					this.dataSource = 'P'
		        }
			    if(this.dataSource==='P'){
				    this.unitList = "UNIT = "+this.popup.features[0].attributes.UNIT;
			        if (this.popup.features.length > 1) {
				        for (var ii = 1; ii < this.popup.features.length; ii++) {
				            this.unitList += " OR UNIT = "+this.popup.features[ii].attributes.UNIT;
                        }
			        }
			    }else{
				    this.dataSource = 'P';
			    }
			}
          }
		  
          this.displayPopupContent(this.popup.getSelectedFeature());
          this.featureCount.innerHTML = "(1 of " + this.popup.features.length + ")";

          //enable navigation if more than one feature is selected
          if(this.popup.features.length > 1){
            domUtils.show(this.pager);
            domClass.add(this.previous, "hidden");
            domClass.remove(this.next, "hidden");
            domClass.remove(this.btnClearSel, "hidden");
			//domClass.add(this.btnClear, "hidden");
			//domClass.remove(this.btnClearAll, "hidden");
          }else if (this.popup.features.length === 1){
            domUtils.show(this.pager);
            domClass.add(this.previous, "hidden");
            domClass.add(this.next, "hidden");
            domClass.add(this.btnClearSel, "hidden");
			//domClass.add(this.btnClearAll, "hidden");
			//domClass.remove(this.btnClear, "hidden");
          }else{
            domUtils.hide(this.pager);
            domClass.add(this.btnClearSel, "hidden");
          }
        })));
        //this.own(on(this.previous, "click", lang.hitch(this, function(){this.selectPrevious();})));
		//this.own(on(this.next, "click", lang.hitch(this, function(){this.selectNext();})));
		this.own(on(this.previous, "click", lang.hitch(this, this.selectPrevious)));
        this.own(on(this.next, "click", lang.hitch(this, this.selectNext)));
        this.own(on(this.btnClear, "click", lang.hitch(this, this.clearResults)));
        this.own(on(this.zt, "click", lang.hitch(this, this.zoomToClicked)));
		this.own(on(this.pt, "click", lang.hitch(this, this.panToClicked)));
		this.own(on(this.pr, "click", lang.hitch(this, this.printClicked)));
        this.own(on(this.btnClearSel, "click", lang.hitch(this, this.clearSelResults)));
        this.own(on(window, 'resize', lang.hitch(this, this.onWindowResize)));
      },
	  
	  selectPrevious: function () {
        this.popup.selectPrevious();
        this.featureCount.innerHTML = "(" + (this.popup.selectedIndex + 1) + " of " + this.popup.features.length + ")";
        if((this.popup.selectedIndex + 1) < this.popup.features.length){
          domClass.remove(this.next, "hidden");
        }
        if(this.popup.selectedIndex === 0){
          domClass.add(this.previous, "hidden");
        }
		this.displayPopupContent(this.popup.getSelectedFeature());
      },

      selectNext: function () {
        domClass.remove(this.previous, "hidden");
        this.popup.selectNext();
        this.featureCount.innerHTML = "(" + (this.popup.selectedIndex + 1) + " of " + this.popup.features.length + ")";
        if((this.popup.selectedIndex + 1) === this.popup.features.length){
          domClass.add(this.next, "hidden");
        }
		this.displayPopupContent(this.popup.getSelectedFeature());
      },
	  
	  clearResults: function() {
        if(this.config.closeOnClear){
          this.closeWidget();
        }else{
		  this.preHeight = 100;
		  var pm = PanelManager.getInstance().getPanelById(this.id + '_panel');
		  pm.resize({h:this.preHeight});
		}
        if(this.popupContent){
          this.popupContent.set("content", "");
        }
        domUtils.hide(this.pager);
        domUtils.hide(this.actionsPaneDiv);
		domUtils.show(this.instructions);
		if (this.fSearch){
			  this.fSearch = false;
			  this.publishData({
                  message: "clearSearch"
		      });
		}
        this.popup.clearFeatures();
		this.dataSource = 'P';
		this.unitList = null;
      },
	  
	  zoomToClicked: function() {
		//**zoom respects the limits for the current para unit
		if (this.dataSource==='P' || this.dataSource==='B'){
			this.map.centerAndZoom(this.popup.getSelectedFeature().geometry.getCentroid(),this.zoomLOD[this.curUnitID]);
		}else{
			var graphicExtent = this.popup.getSelectedFeature().geometry.getExtent().expand(this.config.zoomFactor);
		    this.map.setExtent(graphicExtent,true);
		}
      },

	  panToClicked: function() {
		this.map.centerAt(this.popup.getSelectedFeature().geometry.getCentroid());
      },
	  
	  printClicked: function() {
		var newWindow = window.open("","InfoWindow","titlebar=no,menubar=yes");
		var curDate = new Date().toString().slice(0,16);
		if (this.popup.features){
			var xA = this.popup.getSelectedFeature().attributes;
			if (xA.sS){
			    this.xNum = xA.SAMPLES;
			    this.xLocation = xA.Location;
		    }else{
			    this.xNum = 9;
				this.xLocation = xA.Location;
			}
		}
		if (this.xNum>0){
			var myContent = "<body style='font-family: Arial,Helvetica,sans-serif'>";
			myContent += "<h3>"+this.paraTitle+" for "+this.xLocation+"</h3><table frame='box'>";
		    if (this.paraTitle == "BACTERIA (Positive/Negative)"){
			    myContent += "<tr><th><b>Bacteria</b></th><th><b>Number</b></th><th><b>Percent</b></th></tr>"+
		                 "<tr><td>"+this.paraRanges[0]+"</td><td style='text-align: right'>"+this.getInt(xA.BACT_POS)+"</td><td style='text-align: right'>"+this.getPC(xA.BACT_POS, xA.BACT_SAMP)+"</td></tr>"+
					     "<tr><td>"+this.paraRanges[1]+"</td><td style='text-align: right'>"+this.getInt(xA.BACT_SAMP-xA.BACT_POS)+"</td><td style='text-align: right'>"+this.getPC(xA.BACT_SAMP-xA.BACT_POS, xA.BACT_SAMP)+"</td></tr>"+
					     "<tr><td style='text-align: right'><b>"+this.paraRanges[2]+"</b></td><td style='text-align: right'>"+this.getInt(xA.BACT_SAMP)+"</td></tr>"+
					     "<tr><td>"+this.paraRanges[3]+"</td><td style='text-align: right'>"+this.getInt(xA.ECOLI_POS)+"</td><td style='text-align: right'>"+this.getPC(xA.ECOLI_POS, xA.ECOLI_SAMP)+"</td></tr>"+
					     "<tr><td>"+this.paraRanges[4]+"</td><td style='text-align: right'>"+this.getInt(xA.ECOLI_SAMP-xA.ECOLI_POS)+"</td><td style='text-align: right'>"+this.getPC(xA.ECOLI_SAMP-xA.ECOLI_POS, xA.ECOLI_SAMP)+"</td></tr>"+
					     "<tr><td style='text-align: right'><b>"+this.paraRanges[5]+"</b></td><td style='text-align: right'>"+this.getInt(xA.ECOLI_SAMP)+"</td></tr></table>";
		    }else{
		        myContent += "<tr><th><b>Range</b></th><th><b>Number</b></th><th><b>Percent</b></th><th><b>Summary</b></th></tr>"+
		                 "<tr><td>"+this.paraRanges[0]+"</td><td style='text-align: right'>"+this.getInt(xA.BIN1)+"</td><td style='text-align: right'>"+this.getPC(xA.BIN1, xA.SAMPLES)+"</td><td style='padding-left: 20px'>Minimum: "+this.getSum(xA.MINIMUM)+"</td></tr>"+
					     "<tr><td>"+this.paraRanges[1]+"</td><td style='text-align: right'>"+this.getInt(xA.BIN2)+"</td><td style='text-align: right'>"+this.getPC(xA.BIN2, xA.SAMPLES)+"</td></tr>"+
					     "<tr><td>"+this.paraRanges[2]+"</td><td style='text-align: right'>"+this.getInt(xA.BIN3)+"</td><td style='text-align: right'>"+this.getPC(xA.BIN3, xA.SAMPLES)+"</td><td style='padding-left: 20px'>Median: "+this.getSum(xA.MEDIAN)+"</td></tr>"+
					     "<tr><td>"+this.paraRanges[3]+"</td><td style='text-align: right'>"+this.getInt(xA.BIN4)+"</td><td style='text-align: right'>"+this.getPC(xA.BIN4, xA.SAMPLES)+"</td><td style='padding-left: 20px'>Average: "+this.getSum(xA.AVERAGE)+"</td></tr>"+
					     "<tr><td>"+this.paraRanges[4]+"</td><td style='text-align: right'>"+this.getInt(xA.BIN5)+"</td><td style='text-align: right'>"+this.getPC(xA.BIN5, xA.SAMPLES)+"</td></tr>"+
					     "<tr><td>"+this.paraRanges[5]+"</td><td style='text-align: right'>"+this.getInt(xA.BIN6)+"</td><td style='text-align: right'>"+this.getPC(xA.BIN6, xA.SAMPLES)+"</td><td style='padding-left: 20px'>Maximum: "+this.getSum(xA.MAXIMUM)+"</td></tr>"+
					     "<tr><td style='text-align: right'><b>Total Samples:</b></td><td style='text-align: right'>"+this.getInt(xA.SAMPLES)+"</td></tr>";
		        if (this.paraLimits[0] != "0"){
			        myContent += "<tr><td>"+this.paraLimits[1]+"</td><td style='text-align: right'>"+this.getInt(this.paraX)+"</td><td style='text-align: right'>"+this.getPC(this.paraX, xA.SAMPLES)+"</td><td style='padding-left: 20px'>"+this.paraLimits[2]+"</td></tr></table>";
		        }else{
			        myContent += "</table>";  
		        }
		    }
			myContent += "<p style='font-size: 9px'>NOTE: This groundwater quality summary may not be representative of the actual groundwater quality for the area selected. It is<br>"+
		                 "based on private well samples that are not a statistically random distribution for the area.  Do not extrapolate these statistics to specific<br>"+
						 "areas or addresses.  The Center for Watershed Science and Education is not responsible for the use or application of this summary.</p>"+
                         "<p>Groundwater Center - Center for Watershed Science and Education - UWSP<br>Public Web Mapping Service: "+curDate+"</p>";
		}else if(this.xNum == 0){
			var myContent = "<p>No Samples Found for "+this.paraTitle+" for "+this.xLocation+"</p>";
			myContent += "<p>Groundwater Center - Center for Watershed Science and Education - UWSP<br>Public Web Mapping Service: "+curDate+"</p>";
		}else{
			var myContent = "<p>"+this.xLocation+"</p>";
			myContent += "<p>Groundwater Center - Center for Watershed Science and Education - UWSP<br>Public Web Mapping Service: "+curDate+"</p>";
        }						  
		newWindow.document.write(myContent);
	  },
	  
	  clearSelResults: function(){
        var curFeats = this.popup.features;
        curFeats.splice(this.popup.selectedIndex, 1);
		this.dataSource = "P";
        this.popup.setFeatures(curFeats);
      },
	  
	  onReceiveData: function(name, widgetId, data) {
        if(name === 'LayerList'){
			this.curLayerID = data.message;
		    var oldUnitID = this.curUnitID;
		    this.curUnitID = ((this.curLayerID-this.baseLayers)/this.chemLayers)|0;
			var layerRef=this.curLayerID-this.baseLayers-((this.curUnitID)*this.chemLayers);
		    this.paraTitle = this.config.paras[layerRef].title;
		    this.paraRanges = this.config.paras[layerRef].ranges.split(",");
		    this.paraLimits = this.config.paras[layerRef].limits.split(",");
		    this.paraDec = Math.pow(10, this.config.paras[layerRef].dec);
			if(!this.fSearch && this.unitList) {
		        if (oldUnitID != this.curUnitID){
			        this.clearResults();
			    }else{
				    this.dataSource = "Q";
				    this.getNewPara();
			    }
		    }
		}//else if(data.message === 'fromSearch'){
			//this.fSearch = true;
		//}
      },
	  
	  getNewPara: function(){
		    //var paraURL = "https://gissrv2.uwsp.edu/ArcGIS/rest/services/Pri_Wells/MapServer/"+(this.curLayerID+this.chemOffset);
			var paraURL = this.config.baseURL+(this.curLayerID+this.chemOffset);
		    var queryTask = new QueryTask(paraURL);
            var query = new Query();
            query.returnGeometry = true;
            query.outFields = ["*"];
			query.where = this.unitList;
		    queryTask.execute(query,lang.hitch(this, this._onSearchFinish),
                lang.hitch(this, this._onSearchError));
	  },
	  
	  _onSearchFinish: function(results){
		if (results.features.length > 0){
			this.popup.setFeatures(results.features);
		}else{
			domUtils.hide(this.pager);
			this.dataSource = 'P';
			this.xNum = 0;
			this.xLocation = "current selection";
			this.displayPopupContent();
		}	  
	  },
	  
	  _onSearchError: function(error){
		  domUtils.hide(this.pager);
		  this.dataSource = 'P';
		  this.xNum = -9;
		  this.xLocation = "Error querying for new parameter; please report if persistent";
		  this.displayPopupContent();
	  },

      displayPopupContent: function (feature) {
          PanelManager.getInstance().openPanel(this.inPanel);
		  //PanelManager.getInstance().normalizePanel(this.inPanel);
		  //var pConfig = this.getWidgetConfig("_25");
		  //PanelManager.getInstance().showPanel(pConfig[0]);
		  var hPixels
		  if (feature){
			  var xA = feature.attributes;
			  if (xA.sS){
			      this.xNum = xA.SAMPLES;
			      this.xLocation = xA.Location;
		      }else{
				  this.xNum = 9;
				  this.xLocation = xA.Location;
			  }
		  }
		  if (this.xNum>0){
			hPixels = 280+((((this.paraTitle.length+this.xLocation.length+4)/this.lineBreak)|0)*18);
		    var myContent = "<div class=pTitle>"+this.paraTitle+" for "+this.xLocation+"</div><table class=pTab>";
		    if (this.paraTitle == "BACTERIA (Positive/Negative)"){
			  hPixels -= 36;
			  myContent += "<tr><th><b>Bacteria</b></th><th><b>Number</b></th><th><b>Percent</b></th></tr>"+
		                 "<tr><td>"+this.paraRanges[0]+"</td><td class=pNum>"+this.getInt(xA.BACT_POS)+"</td><td class=pNum>"+this.getPC(xA.BACT_POS, xA.BACT_SAMP)+"</td></tr>"+
					     "<tr><td>"+this.paraRanges[1]+"</td><td class=pNum>"+this.getInt(xA.BACT_SAMP-xA.BACT_POS)+"</td><td class=pNum>"+this.getPC(xA.BACT_SAMP-xA.BACT_POS, xA.BACT_SAMP)+"</td></tr>"+
					     "<tr><td class=pNum><b>"+this.paraRanges[2]+"</b></td><td class=pNum>"+this.getInt(xA.BACT_SAMP)+"</td></tr>"+
					     "<tr><td>"+this.paraRanges[3]+"</td><td class=pNum>"+this.getInt(xA.ECOLI_POS)+"</td><td class=pNum>"+this.getPC(xA.ECOLI_POS, xA.ECOLI_SAMP)+"</td></tr>"+
					     "<tr><td>"+this.paraRanges[4]+"</td><td class=pNum>"+this.getInt(xA.ECOLI_SAMP-xA.ECOLI_POS)+"</td><td class=pNum>"+this.getPC(xA.ECOLI_SAMP-xA.ECOLI_POS, xA.ECOLI_SAMP)+"</td></tr>"+
					     "<tr><td class=pNum><b>"+this.paraRanges[5]+"</b></td><td class=pNum>"+this.getInt(xA.ECOLI_SAMP)+"</td></tr></table>";
		    }else{
		      var paraCnts = [xA.BIN1,xA.BIN2,xA.BIN3,xA.BIN4,xA.BIN5,xA.BIN6];
		      myContent += "<tr><th><b>Range</b></th><th><b>Number</b></th><th><b>Percent</b></th><th><b>Summary</b></th></tr>"+
		                 "<tr><td>"+this.paraRanges[0]+"</td><td class=pNum>"+this.getInt(xA.BIN1)+"</td><td class=pNum>"+this.getPC(xA.BIN1, xA.SAMPLES)+"</td><td class=pSum>Minimum: "+this.getSum(xA.MINIMUM)+"</td></tr>"+
					     "<tr><td>"+this.paraRanges[1]+"</td><td class=pNum>"+this.getInt(xA.BIN2)+"</td><td class=pNum>"+this.getPC(xA.BIN2, xA.SAMPLES)+"</td></tr>"+
					     "<tr><td>"+this.paraRanges[2]+"</td><td class=pNum>"+this.getInt(xA.BIN3)+"</td><td class=pNum>"+this.getPC(xA.BIN3, xA.SAMPLES)+"</td><td class=pSum>Median: "+this.getSum(xA.MEDIAN)+"</td></tr>"+
					     "<tr><td>"+this.paraRanges[3]+"</td><td class=pNum>"+this.getInt(xA.BIN4)+"</td><td class=pNum>"+this.getPC(xA.BIN4, xA.SAMPLES)+"</td><td class=pSum>Average: "+this.getSum(xA.AVERAGE)+"</td></tr>"+
					     "<tr><td>"+this.paraRanges[4]+"</td><td class=pNum>"+this.getInt(xA.BIN5)+"</td><td class=pNum>"+this.getPC(xA.BIN5, xA.SAMPLES)+"</td></tr>"+
					     "<tr><td>"+this.paraRanges[5]+"</td><td class=pNum>"+this.getInt(xA.BIN6)+"</td><td class=pNum>"+this.getPC(xA.BIN6, xA.SAMPLES)+"</td><td class=pSum>Maximum: "+this.getSum(xA.MAXIMUM)+"</td></tr>"+
					     "<tr><td><b>Total Samples:</b></td><td class=pNum>"+this.getInt(xA.SAMPLES)+"</td></tr>";
		      if (this.paraLimits[0] != "0"){
			    var xX = 0;
			    var xStart = parseInt(this.paraLimits[0])-1;
			    for (var i = xStart; i < 6; i++) {
				   xX += paraCnts[i];
                }
			    this.paraX=xX;
			    myContent += "<tr><td>"+this.paraLimits[1]+"</td><td class=pNum>"+this.getInt(xX)+"</td><td class=pNum>"+this.getPC(xX, xA.SAMPLES)+"</td><td class=pSum>"+this.paraLimits[2]+"</td></tr></table>";
		      }else{
			    myContent += "</table>";
				hPixels -= 18;
		      }
		    }
		  }else if(this.xNum == 0){
			  hPixels=280;
			  var myContent = "<div class=pTitle>"+"No Samples Found for "+this.paraTitle+" for "+this.xLocation+"</div><table class=pTab>";
		  }else{
			  hPixels=280;
			  var myContent = "<div class=pTitle>"+this.xLocation+"</div><table class=pTab>";
		  }	
		  if (hPixels != this.preHeight){
			    this.preHeight = hPixels;
				var pm = PanelManager.getInstance().getPanelById(this.id + '_panel');
				pm.resize({h:hPixels});
		  }
          this.popupContent.set("content", myContent);
      },
	  
	  getSum: function(sum){
		  if (sum==0){
		      return "No Detect";
		  }else if (this.paraDec==0){
		      return Math.round(sum);
		  }else{
			  return Math.round((sum+0.00001)*this.paraDec)/this.paraDec;
		  }
	  },
	  
	  getPC: function(sub, tot){
		  if (sub == 0){
			  return "0%";
		  }else{
		      var paraPC = sub/tot*100;
		      if (paraPC<1) {
			      return "<1%";
		      }else{
			      return Math.round(paraPC)+"%";
		      }
		  }
	  },
	  
	  getInt: function(oriValue){
		  var parts = oriValue.toString().split(".");
		  //this is apparently a regrex??
		  return parts[0].replace(/\B(?=(\d{3})+(?!\d))/g,",");
		  //return oriValue|0;
	  },
	  
      onDestroy: function () {
		//does not appear to occur in this version alert("destroy");
        var mapMan = MapManager.getInstance();
        mapMan.resetInfoWindow(false);
        if(!mapMan.isMobileInfoWindow){
		  //alert("destroy-mobile");
          this.map.infoWindow.set("popupWindow", true);
        }
      }

    });
  });
