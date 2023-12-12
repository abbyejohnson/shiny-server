///////////////////////////////////////////////////////////////////////////
// Dave Mechenich custom search (used some elements of esearch wisget by Robert Scheitlin
///////////////////////////////////////////////////////////////////////////
/*global define, dojo, console, window, setTimeout, jimuConfig*/
define([
    'dojo/_base/declare',
    'dijit/_WidgetsInTemplateMixin',
    'jimu/BaseWidget',
    'jimu/dijit/TabContainer',
    'jimu/utils',
    'esri/tasks/query',
    'esri/tasks/QueryTask',
    'esri/tasks/BufferParameters',
	'esri/tasks/AreasAndLengthsParameters',
    'esri/tasks/GeometryService',
    'esri/config',
    'esri/graphic',
    'esri/graphicsUtils',
    'esri/geometry/Point',
    'esri/geometry/Polygon',
    'esri/geometry/Extent',
    'esri/toolbars/draw',
	'esri/dijit/LocateButton',
	'esri/dijit/Search',
    'dojo/Deferred',
    'dijit/ProgressBar',
    'dojo/_base/lang',
    'dojo/on',
    'dojo/_base/html',
	'dojo/i18n!esri/nls/jsapi',
    'dijit/form/Select',
    'dijit/form/NumberTextBox',
    'jimu/dijit/DrawBox',
    'jimu/dijit/LoadingShelter',
    'esri/SpatialReference',
    'dojo/aspect',
    'esri/domUtils',
	'esri/geometry/scaleUtils',
    'dijit/form/DropDownButton',
    'dijit/Menu',
    'dijit/MenuItem'
  ],
  function (
    declare, _WidgetsInTemplateMixin, BaseWidget, TabContainer, jimuUtils, Query, QueryTask,
    BufferParameters, AreasAndLengthsParameters, GeometryService,
    esriConfig, Graphic, graphicsUtils, Point, Polygon, Extent,
    Draw, LocateButton, Search, Deferred, ProgressBar, lang, on, html,
    esriBundle, Select, NumberTextBox, DrawBox, LoadingShelter, SpatialReference,
    aspect, domUtils, scaleUtils
  ) { /*jshint unused: true*/
    return declare([BaseWidget, _WidgetsInTemplateMixin], {
	  curLayerID: 11,
	  curUnitID: 0,
	  curParaID: 11,
	  maxChem: null,
	  baseLayers: null,
	  chemLayers: null,
      name: 'Search',
      label:'Select-Locate',
      baseClass: 'widget-esearch',
      graphicGeometry: null,
      AttributeLayerIndex: 0,
	  ptMethodIndex: 0,
	  zoomLOD: 3,
	  zoomScale: 1155581.108577,
	  sSource: null,
	  minArea: 1,
	  maxR: 30,
	  mcdGroup: "T",
      progressBar: null,
	  progressBar2: null,
	  progressBar3: null,
      tabContainer: null,
	  slLocation: null,
	  slWhere: null,
      selTab: null,
      bufferWKID: null,
      initiator: null,
	  mcdLayerIndex: null,
	  mcdTC: false,
	  mcdTownCoounty: null,
      eSelectTypeVal: 'new',

      postCreate: function () {
        this.inherited(arguments);
        this._initTabContainer();
		this._initDrawBox();
        this._initLayerSelect();
        this._iniCurLoc();
		this._iniAddSearch();
        this._initProgressBar();
		this.bufferWKID = this.config.bufferWKID || 102003;
		//**remove defaults from layer counts
		this.baseLayers = this.config.baseLayerCnt;
		this.chemLayers = this.config.chemLayerCnt;
		//
		this.maxChem = this.config.maxChem || 30000;
		this.own(on(this.btnUniqueCancel, "click", lang.hitch(this, this.onUniqueCancel)));
		//this.own(on(window, 'resize', lang.hitch(this, this.onWindowResize)));
      },

	  //onWindowResize: function(){
	  //	  alert("on resize");
	  	  //this.drawBoxText.deactivate();
	  //},
	  
	  startup: function(){
		//alert("startup");
        this.inherited(arguments);
        this.fetchData();
      },
	  
      onReceiveData: function(name, widgetId, data) {
		if(name === 'LayerList'){
            this.curLayerID = data.message-this.baseLayers;
		    this.curUnitID = ((this.curLayerID)/this.chemLayers)|0;
			var oldParaID = this.curParaID;
			this.curParaID = this.curLayerID-((this.curUnitID)*this.chemLayers);
			if(oldParaID != this.curParaID){
				if(this.graphicGeometry){
					switch (this.selTab){
		                case "By Shape":
			                html.setStyle(this.progressBar.domNode, 'display', 'block');
				            break;
			            case "By Text":
			                html.setStyle(this.progressBar2.domNode, 'display', 'block');
				            break;
						case "By Point":
			                html.setStyle(this.progressBar3.domNode, 'display', 'block');
				    }
					//**added ver 3.1
					if(this.slLocation.substr(0,4)==="NOTE"){
						if(this.config.drawLocDetails === "no"){
							this.slLocation = "a custom area";
				            this.search();
			            }else{
				            this.searchArea();
			            }
					}else{
					//**
						this.search();
					}
				}
			}
		}else if (data.message === 'clearSearch'){
			this.graphicGeometry = null;
		}
      },
	  
	  onOpen: function () {
		//alert("onOpen");
      },
	  
	  onClose: function () {
		//alert("onClose");
		this.drawBox.deactivate();
		this.drawBoxPt.deactivate();
		this.drawBoxText.deactivate();
      },
	  
	  _initTabContainer: function () {
        var initView = this.config.initialView || "By Shape";
        this.selTab = initView;

        var tabs = [];
        tabs.push({
            title: "By Shape",
            content: this.tabNode1
        });
        html.replaceClass(this.tabNode1, 'search-tab-node', 'search-tab-node-hidden');
        tabs.push({
            title: "By Text",
            content: this.tabNode2
        });
        html.replaceClass(this.tabNode2, 'search-tab-node', 'search-tab-node-hidden');
        tabs.push({
            title: "By Point",
            content: this.tabNode3
        });
        html.replaceClass(this.tabNode3, 'search-tab-node', 'search-tab-node-hidden');
        this.tabContainer = new TabContainer({
          tabs: tabs,
          selected: this.selTab
        }, this.tabSearch);

        this.tabContainer.startup();
        this.own(on(this.tabContainer, "tabChanged", lang.hitch(this, function (title) {
          this.selTab = title;
		  this.drawBox.deactivate();
		  this.drawBoxPt.deactivate();
		  this.drawBoxText.deactivate();
        })));
        jimuUtils.setVerticalCenter(this.tabContainer.domNode);
      },
	  
	  _initDrawBox: function () {
		esriBundle.toolbars.draw.addPoint = "Click to Identify";
        //aspect.before(this.drawBox, "_activate", lang.hitch(this, function(){
        //  this.publishData({message: "Deactivate_DrawTool"});
        //}));
        this.drawBox.setMap(this.map);
        var enabledButtons = [];
        enabledButtons.push('EXTENT');
        enabledButtons.push('CIRCLE');
        enabledButtons.push('POLYGON');
        this.drawBox.geoTypes = enabledButtons;
        this.drawBox._initTypes();
        if(this.config.keepgraphicalsearchenabled){
          this.drawBox.deactivateAfterDrawing = false;
        }
        this.own(on(this.drawBox, 'DrawEnd', lang.hitch(this, function (graphic) {
			this.initiator = 'graphic';
			this.zoomLOD = this.config.customZoomLOD;
			this.zoomScale = this.config.customZoomScale;
			this.sSource = "S";
			this.minArea = this.config.drawMinArea;
			this.slLocation = "a custom area";
			this.slWhere = "XY_CELL <= "+this.config.maxCell;
			html.setStyle(this.progressBar.domNode, 'display', 'block');
			if(this.eSelectTypeVal === "new" || (this.eSelectTypeVal === "add" && !this.graphicGeometry)){
				this.graphicGeometry = graphic.geometry;
				this.drawBox.clear();
				if(this.config.drawLocDetails === "no"){
				    this.search();
			    }else{
				    this.searchArea();
			    }
			}else{
				var tmpGraphic = graphic.geometry;
				this.drawBox.clear();
				this.graphicAddRem(tmpGraphic);
			}
        })));
		//this is separate point tool for point search
        this.drawBoxPt.setMap(this.map);
        var enabledButtonsPt = [];
        enabledButtonsPt.push('POINT');
        this.drawBoxPt.geoTypes = enabledButtonsPt;
        this.drawBoxPt._initTypes();
		if(this.config.keepPtCenterEnabled){
          this.drawBoxPt.deactivateAfterDrawing = false;
        }
		aspect.before(this.drawBoxPt, "_activate", lang.hitch(this, function(){
            this.drawBoxPt.clear();
        }));
        this.own(on(this.drawBoxPt, 'DrawEnd', lang.hitch(this, function (graphic) {
			document.getElementById("lon").value = Math.round(graphic.geometry.getLongitude()*1000)/1000;
			document.getElementById("lat").value = Math.round(graphic.geometry.getLatitude()*1000)/1000;
		    this.geoLocate.clear();
		    this.addLocate.clear();
        })));
		//this is text search name point tool
        this.drawBoxText.setMap(this.map);
        this.drawBoxText.geoTypes = enabledButtonsPt;
        this.drawBoxText._initTypes();
		if(this.config.keepPtInfoEnabled){
          this.drawBoxText.deactivateAfterDrawing = false;
        }
        this.own(on(this.drawBoxText, 'DrawEnd', lang.hitch(this, function (graphic) {
			this.getSearchText(graphic);
			this.drawBoxText.clear();
        })));
      },
	  
	  _initLayerSelect: function () {
		var attribOptions = [];
        var len = this.config.layers.length;
        for (var i = 0; i < len; i++) {
            if(this.config.layers[i].textSearchLayer){
                var option = {
                    value: i,
                    label: this.config.layers[i].name
                };
			    attribOptions.push(option);
				if (option.label === "WI Counties"){
				    var queryParams = new Query();
		            queryParams.where = "DNR_CTY_NO > 0";
                    var fields = ["DNR_CTY_NO","CTY_NAME"];
                    queryParams.outFields = fields;
				    queryParams.returnGeometry = false;
                    var queryTask = new QueryTask(this.config.layers[i].url);
					//alert(this.config.layers[i].url);
                    queryTask.execute(queryParams, lang.hitch(this, this._onCoSearchFinish),
                        lang.hitch(this, this._onDDSearchError));
				}
				if (option.label === "WI Municipal"){
					this.mcdLayerIndex=i;
				}
			}
        }
		attribOptions[0].selected = true;
		this.AttributeLayerIndex = attribOptions[0].value;
		this.selectLayerAttribute.addOption(attribOptions);
		html.setStyle(this.trText, 'display', 'none');
		html.setStyle(this.trsText, 'display', 'none');
		html.setStyle(this.mcdText, 'display', 'none');
		html.setStyle(this.mcdVdisplay, 'display', 'none');
		html.setStyle(this.mcdCdisplay, 'display', 'none');
		html.setStyle(this.mcdUnique, 'display', 'none');
		html.setStyle(this.hucText, 'display', 'none');
		html.setStyle(this.gwuText, 'display', 'none');
		this.own(on(this.selectLayerAttribute, "change", lang.hitch(this, this.onAttributeLayerChange)));
	  },
	  
	  onAttributeLayerChange: function (newValue) {
        this.AttributeLayerIndex = newValue;
		//this.drawBoxText.deactivate();
		html.setStyle(this.coText, 'display', 'none');
		html.setStyle(this.trText, 'display', 'none');
		html.setStyle(this.trsText, 'display', 'none');
		html.setStyle(this.mcdText, 'display', 'none');
		html.setStyle(this.mcdUnique, 'display', 'none');
		html.setStyle(this.hucText, 'display', 'none');
		html.setStyle(this.gwuText, 'display', 'none');
		switch (this.config.layers[this.AttributeLayerIndex].name){
		    case "WI Counties":
			    html.setStyle(this.coText, 'display', 'block');
				break;
			case "WI Township Range":
			    html.setStyle(this.trText, 'display', 'block');
				break;
			case "WI Sections":
			    html.setStyle(this.trText, 'display', 'block');
			    html.setStyle(this.trsText, 'display', 'block');

				break;
			case "WI Municipal":
			    if (this.selectMCDT.options.length === 0) {
					//this.selectMCDT.set("multiCharSearchDuration", 15000000);
					this.populateMCDs("T");
					this.own(on(this.selectMCDT, "click", lang.hitch(this, this.onMCDTChange)));
					html.setStyle(this.mcdText, 'display', 'block');
					this.populateMCDs("V");
					this.populateMCDs("C");
				}else{
			        html.setStyle(this.mcdText, 'display', 'block');
				}
				break;
			case "WI HUC10 Watersheds":
			    if (this.selectWatershed.options.length === 0) {
					this.populateHUCs();
				}
			    html.setStyle(this.hucText, 'display', 'block');
				break;	
			case "WI Groundwater Units":
			    if (this.selectDistrict.options.length === 0) {
					this.populateGWUs();
				}
			    html.setStyle(this.gwuText, 'display', 'block');	
		}
      },
	  
	  _dirSelected: function() {
		if (document.getElementById("eDir").checked) {
			this.maxR = 30;
		}else{
			this.maxR = 20;
		}
		var xnRange = parseInt(document.getElementById("range").value);
		if (xnRange>this.maxR) {
			document.getElementById("range").value = this.maxR;
		}
	  },

	  _mcdSelected: function() {
		html.setStyle(this.mcdUnique, 'display', 'none');
		html.setStyle(this.mcdTdisplay, 'display', 'none');
		html.setStyle(this.mcdVdisplay, 'display', 'none');
		html.setStyle(this.mcdCdisplay, 'display', 'none');
		if (document.getElementById("mcdT").checked) {
			this.mcdGroup = "T";
			html.setStyle(this.mcdTdisplay, 'display', 'block');
			//this.populateMCDs();
		}else if (document.getElementById("mcdV").checked){
			this.mcdGroup = "V";
			html.setStyle(this.mcdVdisplay, 'display', 'block');
			//this.populateMCDs();
		}else{
			this.mcdGroup = "C";
			html.setStyle(this.mcdCdisplay, 'display', 'block');
			//this.populateMCDs();
		}
	  },
	  
	  onMCDTChange: function(){
		  //alert("T Change");
		  this.mcdTC = false;
	  },
	  
	  onUniqueCancel: function() {
		  html.setStyle(this.mcdUnique, 'display', 'none');
		  html.setStyle(this.mcdTdisplay, 'display', 'block');
	  },
	  
	  _tInput: function() {
		var xTownship = document.getElementById("township").value;
		if (xTownship.length == 0) {
			document.getElementById("township").value = 1;
		}else{
			var xnTownship = parseInt(xTownship);
		    if (xnTownship<1) {
			    document.getElementById("township").value = 1;
		    }else if(xnTownship>53) {
				document.getElementById("township").value = 53;
			}else{
				document.getElementById("township").value = xnTownship;
			}
		}
	  },
	  
	  _rInput: function() {
		var xRange = document.getElementById("range").value;
		if (xRange.length == 0) {
			document.getElementById("range").value = 1;
		}else{
			var xnRange = parseInt(xRange);
		    if (xnRange<1) {
			    document.getElementById("range").value = 1;
		    }else{
				document.getElementById("range").value = xnRange;
			}
		}
	  },
	  
	  _sInput: function() {
		var xSection = document.getElementById("section").value;
		if (xSection.length == 0) {
			document.getElementById("section").value = 1;
		}else{
			var xnSection = parseInt(xSection);
		    if (xnSection<1) {
			    document.getElementById("section").value = 1;
		    }else if(xnSection>36) {
				document.getElementById("section").value = 36;
			}else{
				document.getElementById("section").value = xnSection;
			}
		}
	  },
	  
	  populateHUCs: function() {
		  var queryParams = new Query();
		  queryParams.where = "1 = 1";
          var fields = ["Name"];
          queryParams.outFields = fields;
		  queryParams.returnGeometry = false;
          var queryTask = new QueryTask(this.config.layers[this.AttributeLayerIndex].url);
          queryTask.execute(queryParams, lang.hitch(this, this._onHUCFinish),
              lang.hitch(this, this._onDDSearchError));
	  },
	  
	  _onHUCFinish: function(results){
		  var resultFeatures = results.features;
		  var hucArray = [];
		  var hucOptions = [];
		  for (i = 0; i < resultFeatures.length; i++) {
			  hucArray.push(resultFeatures[i].attributes.Name);
		  }
		  hucArray.sort();
		  //var prevHUC = "first";
          for (var r = 0; r < hucArray.length; r++) {
			  //if (hucArray[r] != prevHUC){
			  //	prevHUC = hucArray[r];
				var option = {
                    value: hucArray[r],
                    label: hucArray[r]
                };
			    hucOptions.push(option);
			  //}
		  }
		  this.selectWatershed.addOption(hucOptions);
	  },
	  
	  populateGWUs: function() {
		  var queryParams = new Query();
		  queryParams.where = "1 = 1";
          var fields = ["Zones"];
          queryParams.outFields = fields;
		  queryParams.returnGeometry = false;
          var queryTask = new QueryTask(this.config.layers[this.AttributeLayerIndex].url);
          queryTask.execute(queryParams, lang.hitch(this, this._onGWUFinish),
              lang.hitch(this, this._onDDSearchError));
	  },
	  
	   _onGWUFinish: function(results){
		  var resultFeatures = results.features;
		  var gwuArray = [];
		  var gwuOptions = [];
		  for (i = 0; i < resultFeatures.length; i++) {
			  gwuArray.push(resultFeatures[i].attributes.Zones);
		  }
		  gwuArray.sort();
          for (var r = 0; r < gwuArray.length; r++) {
			var option = {
                value: gwuArray[r],
                label: gwuArray[r]
            };
			gwuOptions.push(option);
		  }
		  this.selectDistrict.addOption(gwuOptions);
	  },
	  
	  _onCoSearchFinish: function(results){
		  var resultFeatures = results.features;
		  var coOptions = [];
          for (var r = 0; r < resultFeatures.length; r++) {
		      var option = {
                  value: resultFeatures[r].attributes.DNR_CTY_NO,
                  label: resultFeatures[r].attributes.CTY_NAME
              };
			  coOptions.push(option);  
		  }
		  coOptions.sort(function(a, b){
              return a.value-b.value
          })
		  this.selectCounty.addOption(coOptions);
	  },
	  
	  populateMCDs: function(mcdGrp) {
		  var queryParams = new Query();
		  queryParams.where = "C_T_V = '"+mcdGrp+"'";
          var fields = ["MCD_NAME"];
          queryParams.outFields = fields;
		  queryParams.returnGeometry = false;
          var queryTask = new QueryTask(this.config.layers[this.AttributeLayerIndex].url);
          queryTask.execute(queryParams, lang.hitch(this, this._onMCDFinish, mcdGrp),
              lang.hitch(this, this._onDDSearchError));
	  },
	  
	  _onMCDFinish: function(mcdGrp, results){
		  var resultFeatures = results.features;
		  var mcdArray = [];
		  var mcdOptions = [];
		  for (i = 0; i < resultFeatures.length; i++) {
			  mcdArray.push(resultFeatures[i].attributes.MCD_NAME);
		  }
		  mcdArray.sort();
		  var prevMCD = "first";
          for (var r = 0; r < mcdArray.length; r++) {
			  if (mcdArray[r] != prevMCD){
				  prevMCD = mcdArray[r];
				  var option = {
                      value: mcdArray[r],
                      label: mcdArray[r]
                  };
			      mcdOptions.push(option);
			  }
		  }
		  if(mcdGrp==='T'){
		      this.selectMCDT.addOption(mcdOptions);
		  }else if(mcdGrp==='V'){
			  this.selectMCDV.addOption(mcdOptions);
		  }else{
			  this.selectMCDC.addOption(mcdOptions);
		  }
	  },
	  
	  _onDDSearchError: function(error){
		  this.slLocation = "NOTE: Error populating dropdown select boxes for By Text search";
		  this._onSearchFinish("message");
          //alert("Dopdown Populate Error");
	  },
	  
	  onSearch: function () {
		  this.drawBoxText.deactivate();
		  switch (this.config.layers[this.AttributeLayerIndex].name){
			  case "WI Counties":
			      this.initiator = 'county';			  
	              var sqlText = "DNR_CTY_NO = "+this.selectCounty.value;
				  this.slWhere = "CID = "+this.selectCounty.value;
				  break;
			  case "WI Township Range":
			  case "WI Sections":
				  var xnT = parseInt(document.getElementById("township").value);
                  var xnR = parseInt(document.getElementById("range").value);
		          if (xnR>this.maxR) {
			          document.getElementById("range").value = this.maxR;
					  xnR = this.maxR;
		          }
				  var xnD = 40000;
				  if (document.getElementById("wDir").checked) {
			          xnD = 20000;
		          }
			      var xnDTR = xnD+(xnT*100)+xnR;
				  if (this.config.layers[this.AttributeLayerIndex].name === "WI Township Range") {
					  this.initiator = 'TR';
				      var sqlText = "DTR = "+xnDTR;
				      this.slWhere = "DTR = "+xnDTR;
				  }else{
					  this.initiator = 'section';
					  var xnS = parseInt(document.getElementById("section").value);
					  xnDTR = (xnDTR*100)+xnS;
					  var sqlText = "DTRS = "+xnDTR;
				      this.slWhere = "DTRS = "+xnDTR;
				  }
				  break;
			  case "WI Municipal":
			      var uIsVisible = window.getComputedStyle(this.mcdUnique, null).getPropertyValue("display");
				  if(this.mcdGroup==='T'){
					  if (uIsVisible === "none" && !this.mcdTC){
						  var sqlText = "MCD_NAME = '"+this.selectMCDT.value+"' AND C_T_V = 'T'";
				      }else if(this.mcdTC){
					      var sqlText = "Loc = '"+this.mcdTownCounty+"'";
				      }else{
					      var sqlText = "Loc = '"+this.selectMCDUnique.value+"'";
					      html.setStyle(this.mcdUnique, 'display', 'none');
					      html.setStyle(this.mcdTdisplay, 'display', 'block');
				      } 
				  }else if(this.mcdGroup==='V'){
					  var sqlText = "MCD_NAME = '"+this.selectMCDV.value+"' AND C_T_V = 'V'";
				  }else{
					  var sqlText = "MCD_NAME = '"+this.selectMCDC.value+"' AND C_T_V = 'C'";
				  }
			      this.initiator = 'muni';
				  this.slWhere = "XY_CELL <= "+this.config.maxMCell;
				  break;
			   case "WI HUC10 Watersheds":
			      var sqlText = "Name = '"+this.selectWatershed.value+"'";
			      this.initiator = 'huc';
				  this.slWhere = "XY_CELL <= "+this.config.maxMCell;
                  break;				  
			  case "WI Groundwater Units":
			      var sqlText = "Zones = '"+this.selectDistrict.value+"'";
			      this.initiator = 'gwu';
				  this.slWhere = "XY_CELL <= "+this.config.maxMCell;
		  }
		
        var queryParams = new Query();
		queryParams.where = sqlText;
		var fields = this.config.layers[this.AttributeLayerIndex].fields.all.split(",");
        queryParams.returnGeometry = true;
        queryParams.outSpatialReference = this.map.spatialReference;
        queryParams.outFields = fields;
        html.setStyle(this.progressBar2.domNode, 'display', 'block');
        var queryTask = new QueryTask(this.config.layers[this.AttributeLayerIndex].url);
        queryTask.execute(queryParams, lang.hitch(this, this._onTextSearchFinish),
          lang.hitch(this, this._onTextSearchError));
      },
	  
	  _onTextSearchFinish: function(results){
		  var resultFeatures = results.features;
		  if (results.features.length === 1) {
			  if(this.eSelectTypeVal === "new" || (this.eSelectTypeVal === "add" && !this.graphicGeometry)){
				  this.zoomLOD = this.config.layers[this.AttributeLayerIndex].zoomLOD;
		          this.zoomScale = this.config.layers[this.AttributeLayerIndex].zoomScale;
				  if(this.initiator==='muni' || this.initiator==='gwu' || this.initiator==='huc'){
					  this.sSource = "S";
				  }else{
					  this.sSource = "B";
				  }
				  this.graphicGeometry = resultFeatures[0].geometry;
				  if(this.initiator==='gwu'){
					  this.slLocation = resultFeatures[0].attributes.Zones;
				  }else if(this.initiator==='huc'){
					  this.slLocation = resultFeatures[0].attributes.Name;
				  }else{
					  this.slLocation = resultFeatures[0].attributes.Location;
				  }
		          this.search();
			  }else{
				  this.zoomLOD = this.config.customZoomLOD;
		          this.zoomScale = this.config.customZoomScale;
		          this.sSource = "S";
				  this.initiator = 'graphic';
				  this.slLocation = "a custom area";
                  this.slWhere = "XY_CELL <= "+this.config.maxCell;
				  this.graphicAddRem(resultFeatures[0].geometry);
			  }
		  }else if(resultFeatures.length > 1) {
			  while (this.selectMCDUnique.options.length > 0) {           
                  this.selectMCDUnique.removeOption(0);
              }
			  var uOptions = [];
		      for (i = 0; i < resultFeatures.length; i++) {
				  var option = {
					  //value: i,
                      value: resultFeatures[i].attributes.Loc,
                      label: resultFeatures[i].attributes.Loc
                  };
			      uOptions.push(option);
		      }
			  uOptions.sort(function(a, b){
                  var nameA=a.label, nameB=b.label;
                  if (nameA < nameB){
                      return -1
				  }
                  if (nameA > nameB){
                      return 1
				  }
                  return 0 
              })
		      this.selectMCDUnique.addOption(uOptions);
			  html.setStyle(this.progressBar2.domNode, 'display', 'none');
			  html.setStyle(this.mcdTdisplay, 'display', 'none');
			  html.setStyle(this.mcdUnique, 'display', 'block');
		  }else{	  
			  this.slLocation = "NOTE: Error - No By Text unit boundary returned";
		      this._onSearchFinish("message");
			  //alert("By Text Boundary Error - None returned");
	      }
	  },
	  
	  _onTextSearchError: function(error){
	  	  this.slLocation = "NOTE: Server Error searching for By Text unit boundary";
		  this._onSearchFinish("message");
          //alert("Server By Text Boundary Error");
	  },
	  
	  getSearchText: function(loc){
		var queryParams = new Query();
		queryParams.geometry = loc.geometry;
		var fields = this.config.layers[this.AttributeLayerIndex].fields.all.split(",");
		queryParams.returnGeometry = false;
	    queryParams.outFields = fields;
        var queryTask = new QueryTask(this.config.layers[this.AttributeLayerIndex].url);
		html.setStyle(this.progressBar2.domNode, 'display', 'block');
        queryTask.execute(queryParams, lang.hitch(this, this._onGetSearchText),
            lang.hitch(this, this._ongetSearchTextError));
	  },
	  
	  _onGetSearchText: function(results){
		  var resultFeatures = results.features;
		  if (results.features.length === 0) {
			  return;
		  }
		  switch (this.config.layers[this.AttributeLayerIndex].name){
			  case "WI Counties":
			      this.selectCounty.set("value", resultFeatures[0].attributes.DNR_CTY_NO);
				  break;
			  case "WI Township Range":
			      var sDTR = resultFeatures[0].attributes.DTR.toString();
				  var nDir=parseInt(sDTR.substring(0,1));
				  var nT=parseInt(sDTR.substring(1,3));
				  var nR=parseInt(sDTR.substring(3));
				  document.getElementById("township").value = nT;
				  document.getElementById("range").value = nR;
				  if(nDir===2){
					  //document.getElementById("eDir").checked = false;
					  document.getElementById("wDir").checked = true;
				  }else{
					  //document.getElementById("wDir").checked = false;
					  document.getElementById("eDir").checked = true;
				  }
				  break;
			  case "WI Sections":
			      var sDTRS = resultFeatures[0].attributes.DTRS.toString();
				  var nDir=parseInt(sDTRS.substring(0,1));
				  var nT=parseInt(sDTRS.substring(1,3));
				  var nR=parseInt(sDTRS.substring(3,5));
				  var nS=parseInt(sDTRS.substring(5));
				  document.getElementById("township").value = nT;
				  document.getElementById("range").value = nR;
				  document.getElementById("section").value = nS;
				  if(nDir===2){
					  //document.getElementById("eDir").checked = false;
					  document.getElementById("wDir").checked = true;
				  }else{
					  //document.getElementById("wDir").checked = false;
					  document.getElementById("eDir").checked = true;
				  }
				  break;
			  case "WI Municipal":
			      var sCTV = resultFeatures[0].attributes.C_T_V;
				  //alert(resultFeatures[0].attributes.C_T_V);
				  //alert(resultFeatures[0].attributes.MCD_NAME);
				  if(sCTV==='T'){
					  document.getElementById("mcdT").checked = true;
					  this.selectMCDT.set("value", resultFeatures[0].attributes.MCD_NAME);
					  this.mcdTownCounty = resultFeatures[0].attributes.Loc;
					  this.mcdTC = true;
				  }else if(sCTV==='V'){
					  document.getElementById("mcdV").checked = true;
					  this.selectMCDV.set("value", resultFeatures[0].attributes.MCD_NAME);
				  }else{
					  document.getElementById("mcdC").checked = true;
					  this.selectMCDC.set("value", resultFeatures[0].attributes.MCD_NAME);
				  }
			      this._mcdSelected();
				  break;
			   case "WI HUC10 Watersheds":
                  this.selectWatershed.set("value", resultFeatures[0].attributes.Name);
                  break;				  
			  case "WI Groundwater Units":
				  this.selectDistrict.set("value", resultFeatures[0].attributes.Zones);
		  }
		  html.setStyle(this.progressBar2.domNode, 'display', 'none');
	  },
	  
	  _onGetSearchTextError: function(error){
		  //alert("explore search text error");
	  	  this.slLocation = "NOTE: Query Error searching for Text with Identify";
		  this._onSearchFinish("message");
	  },
	  
	  _iniCurLoc: function(){
		  if(this.config.followPoint) {
			  var json = {"useTracking": false,"centerAt": true,"setScale": false,"highlightLocation": true};
		  }else{
			  var json = {"useTracking": false,"centerAt": false,"setScale": false,"highlightLocation": true};
		  }
		  json.map=this.map;
		  this.geoLocate = new LocateButton(json);
		  html.place(this.geoLocate.domNode, this.pCurLoc);
          this.geoLocate.startup();
		  this.geoLocate.own(on(this.geoLocate, "locate", lang.hitch(this, this.onLocate)));
	  },
	  
	  onLocate: function(parameters){
		  this.drawBoxPt.clear();
		  this.addLocate.clear();
		  document.getElementById("lon").value = Math.round(parameters.graphic.geometry.getLongitude()*1000)/1000;
		  document.getElementById("lat").value = Math.round(parameters.graphic.geometry.getLatitude()*1000)/1000;
	  },
	  
	  _iniAddSearch: function(){
		  this.addLocate = new Search({
            map: this.map,
			enableLabel: false,
            enableInfoWindow: false,
			autoNavigate: false,
			//zoomScale: 300000,
			enableHighlight: true,
			showInfoWindowOnSelect: false,
          });
		 html.place(this.addLocate.domNode, this.searchNode);
         this.addLocate.startup();
		 this.addLocate.own(on(this.addLocate, "select-result", lang.hitch(this, this.onAddress)));
	  },
	  
	  onAddress: function(e){
		  this.drawBoxPt.clear();
		  this.geoLocate.clear();
		  var xLon = Math.round(e.result.feature.geometry.getLongitude()*1000)/1000;
		  var xLat = Math.round(e.result.feature.geometry.getLatitude()*1000)/1000;
		  document.getElementById("lon").value = xLon;
		  document.getElementById("lat").value = xLat;
		  if(this.config.followPoint) {
			  var point = new Point([xLon,xLat],new SpatialReference({ wkid:4326 }));
			  this.map.centerAt(point);
		  }
	  },

	  onSearchPt: function() {
		  this.drawBoxPt.deactivate();
		  var xBuffer = this.bufferValuePt.value;
		  var xLat = document.getElementById("lat").value
		  var xLong = document.getElementById("lon").value
		  var xUnit,xMin,xMax
		  if (document.getElementById("bMiles").checked) {
			  xUnit="UNIT_STATUTE_MILE";
			  xMin=0.5;
			  xMax=50;
		  }else{
			  xUnit="UNIT_KILOMETER";
			  xMin=0.8;
			  xMax=80;
		  }
		  if (isNaN(xBuffer) || xBuffer<xMin || xBuffer>xMax){
			  document.getElementById("buf").focus();
			  this.ptNotice.innerHTML = "Please correct buffer distance (0.5 to 50 miles or 0.8 to 80 km)";
			  html.setStyle(this.ptNotice, 'display', 'block');
			  setTimeout(lang.hitch(this, function () {
					this.hidePtNotice();
              }), 5000);
			  return
		  }
	      if (isNaN(xLat) || xLat<42.3 || xLat>47.3){
			  document.getElementById("lat").focus();
			  this.ptNotice.innerHTML = "Please correct latitude (42.3 to 47.3)";
			  html.setStyle(this.ptNotice, 'display', 'block');
			  setTimeout(lang.hitch(this, function () {
					this.hidePtNotice();
              }), 5000);
			  return
		  }
		  if (isNaN(xLong) || xLong<-93 || xLong>-86.5){
			  document.getElementById("lon").focus();
			  this.ptNotice.innerHTML = "Please correct longitude (-93 to -86.5)";
			  html.setStyle(this.ptNotice, 'display', 'block');
			  setTimeout(lang.hitch(this, function () {
					this.hidePtNotice();
              }), 5000);
			  return
		  }
		  
		  html.setStyle(this.progressBar3.domNode, 'display', 'block');
		  this.initiator = 'graphic';
		  this.zoomLOD = this.config.customZoomLOD;
		  this.zoomScale = this.config.customZoomScale;
		  this.sSource = "S";
		  this.minArea=0.776;
		  this.slLocation = "a custom area";
		  this.slWhere = "XY_CELL <= "+this.config.maxCell;
		  this.drawBoxPt.clear();
		  this.geoLocate.clear();
		  this.addLocate.clear();
		  var point = new Point(xLong,xLat);
		  this._bufferGeometries(point, new SpatialReference({
              wkid: this.bufferWKID
            }), xBuffer, xUnit);
	  },
	  
	  hidePtNotice: function(){
	      html.setStyle(this.ptNotice, 'display', 'none');
	  },
	  
	  _bufferGeometries: function (geomArr, sr, dist, unit) {
        if (geomArr) {
          var bufferParameters = new BufferParameters();
          bufferParameters.geometries = [geomArr];
          bufferParameters.bufferSpatialReference = sr;
          bufferParameters.unit = GeometryService[unit];
          bufferParameters.distances = [dist];
          bufferParameters.unionResults = true;
          bufferParameters.geodesic = false;
          bufferParameters.outSpatialReference = this.map.spatialReference;
          esriConfig.defaults.geometryService.buffer(bufferParameters, lang.hitch(this, function (evt) {
			  if(!evt[0]){
				  this.slLocation = "NOTE: Buffer Geoprocessing error - Please try again (Please report if persistent)";
				  this._onSearchFinish("message");
			  }else{
				  if(this.eSelectTypeVal === "new" || (this.eSelectTypeVal === "add" && !this.graphicGeometry)){
				      this.graphicGeometry = evt[0];
				      if(this.config.drawLocDetails === "no"){
				          this.search();
			          }else{
				          this.searchArea();
			          }
			      }else{
				      this.graphicAddRem(evt[0]);
			      }
		      }
          }));
        }
      },
	  
	  _latlonInput: function(){
		  //alert("ok");
		  this.drawBoxPt.clear();
		  this.addLocate.clear();
		  this.geoLocate.clear();
	  },
	  
	  _bufUnitSelected: function(){
		  if (document.getElementById("bMiles").checked) {
			  this.bufNotice.innerHTML = "Apply a search distance (0.5 to 50 miles):";
		  }else{
			  this.bufNotice.innerHTML = "Apply a search distance (0.8 to 80 kilometers):";
		  }
	  },
	  
	  search: function (spatialRelationship) {
		var queryParams = new Query();
		var pFld = this.config.layers[0].fields.field[this.curParaID].name;
		queryParams.where = this.slWhere+" AND "+pFld+" > -9";
        if (this.initiator === 'graphic' || this.initiator === 'muni' || this.initiator === 'gwu' || this.initiator === 'huc') {
		  //alert("this is graphic");
          queryParams.geometry = this.graphicGeometry;
          queryParams.spatialRelationship = spatialRelationship || Query.SPATIAL_REL_INTERSECTS;
        }
		var fields = [];
        fields.push(pFld);
        queryParams.returnGeometry = false;
        queryParams.outSpatialReference = this.map.spatialReference;
        queryParams.outFields = fields;
		//alert(queryParams.where);
		//alert(queryParams.outFields);
        var queryTask = new QueryTask(this.config.layers[0].url);
		//"https://gissrv2.uwsp.edu/ArcGIS/rest/services/Pri_Wells/MapServer/53");
        queryTask.execute(queryParams, lang.hitch(this, this._onSearchFinish, pFld),
          lang.hitch(this, this._onSearchError));
      },
	  
	  _onSearchError: function (error) {
		this.slLocation = "NOTE: Chemistry Search Error - Please try again (Please report if persistent)";
		this._onSearchFinish("message");
      },

	  _onSearchFinish: function (pFld, results){
		  if (pFld === "message"){
			  
			this.sSource="M";
			var samples = -9;
			var attr = {"Location": this.slLocation,"SAMPLES": samples,"sS": this.sSource};
		  }else{
		    var resultFeatures = results.features;
		    var samples = resultFeatures.length;
		    if (samples == 0){
			  var attr = {"Location": this.slLocation,"SAMPLES": samples,"sS": this.sSource};
			  //alert("nosamples");
		    }else if(samples > (this.maxChem-1)){
			  this.sSource="M";
			  var samples = -9;
			  this.slLocation = "NOTE: Please reduce search area - number of samples exceeds "+this.maxChem+" maximum";
			  var attr = {"Location": this.slLocation,"SAMPLES": samples,"sS": this.sSource};
			  //alert("too many samples");
			}else{
			  if (pFld=="BACT"){
			      var samples2=0;
				  var bPos=0;
				  var bPos2=0;
				  for (i = 0; i < samples; i++) {
					  switch (resultFeatures[i].attributes.BACT){
					      case 0:
						      break;
						  case 2:
						      bPos++;
							  samples2++;
							  break;
						  case 3:
						      bPos++;
							  bPos2++;
							  samples2++;
							  break;
						  case 1:
						      bPos++;
							  break;
					  }
			      }
				  var attr = {"Location": this.slLocation,"SAMPLES": samples,"BACT_SAMP": samples,"ECOLI_SAMP":samples2,"BACT_POS": bPos,"ECOLI_POS": bPos2,"sS": this.sSource};
			  }else{
				  var bin=[0,0,0,0,0,0];
			      var average=0;
			      var median=0;
			      var minimum=0;
			      var maximum=0;
			      var binRanges = this.config.layers[0].fields.field[this.curParaID].bins.split(",").map(Number);
			      var paraData=[];
				  for (i = 0; i < samples; i++) {
					  paraData.push(resultFeatures[i].attributes[pFld]);
				  average += resultFeatures[i].attributes[pFld];
			      }
				  paraData.sort(function(a, b){return a-b});
				  average=average/samples;
				  minimum=paraData[0];
				  maximum=paraData[samples-1];
				  if ((samples % 2) == 0){
					  median = paraData[samples/2];
					  median += paraData[(samples/2)-1];
					  median = median/2;
				  }else{
					  median=paraData[Math.floor(samples/2)];
				  }
				  var binCtr=0;
				  var binIndex=0;
				  var pI=0;
				  while (pI < samples){
					  if (paraData[pI]<=binRanges[binIndex]){
						  binCtr++;
						  pI++;
					  }else{
						  bin[binIndex]=binCtr;
						  binIndex++;
						  binCtr=0;
					  }
				  }
				  bin[binIndex]=binCtr;
				  var attr = {"Location": this.slLocation,"SAMPLES": samples,"AVERAGE": average,"MEDIAN": median,"MINIMUM": minimum,"MAXIMUM": maximum, "BIN1": bin[0],"BIN2": bin[1],"BIN3": bin[2],"BIN4": bin[3],"BIN5": bin[4],"BIN6": bin[5],"sS": this.sSource};
			  }
		    }
			if(this.sSource==='S'){
			    var curExtent = this.map.extent;
		        var cH = curExtent.getHeight();
		        var cW = curExtent.getWidth();
		        var zoomExtent = scaleUtils.getExtentForScale(this.map, this.zoomScale);
		        var zH = zoomExtent.getHeight();
		        var zW = zoomExtent.getWidth();
		        var graphicExtent = this.graphicGeometry.getExtent();
		        var gH = graphicExtent.getHeight();
		        var gW = graphicExtent.getWidth();
		        if(this.config.autoZoomCenter === "center"){
			        if(gH>cH || gW>cW){
				        this.map.setExtent(graphicExtent,true);
			        }else{
				        this.map.centerAt(this.graphicGeometry.getCentroid());
			        }
		        }else if(this.config.autoZoomCenter === "zoom"){
			        var myScale = scaleUtils.getScale(this.map);
			        if(myScale > this.zoomScale){
				        if(gH>zH || gW>zW){
				            this.map.setExtent(graphicExtent,true);
			            }else{
				            this.map.centerAndZoom(this.graphicGeometry.getCentroid(),this.zoomLOD);
			            }
			        }else{
				        if(gH>cH || gW>cW){
				            this.map.setExtent(graphicExtent,true);
			            }else{
				            this.map.centerAt(this.graphicGeometry.getCentroid());
			            }
			        }
		        }
			}else if(this.sSource==='B'){
				var curLOD = this.map.getLevel();
				var sUnit = this.config.layers[this.AttributeLayerIndex].unitNo;
				
				if(sUnit <= 1 && curLOD < this.zoomLOD && this.config.autoZoomCenter === "zoom"){
					this.map.centerAndZoom(this.graphicGeometry.getCentroid(),this.zoomLOD);
					if(sUnit !== this.curUnitID){
						var uBtnID = "u"+sUnit;
						//**var uBtnID = "u"+(sUnit*this.chemLayers);
						this.publishData({
							message: uBtnID
						}, false);
					}
				}else{
					this.map.centerAt(this.graphicGeometry.getCentroid()).then(lang.hitch(this, function() {
						if(sUnit !== this.curUnitID){
							var uBtnID = "u"+sUnit;
							//**var uBtnID = "u"+(sUnit*this.chemLayers);
							this.publishData({
								message: uBtnID
							}, false);
						}
					}));	
				}
			}
		  }
		  //this.publishData({
          //    message: "fromSearch"
          //}, false);
		  var graphic = new Graphic(this.graphicGeometry,null,attr,null);
		  this.map.infoWindow.setFeatures([graphic]);
		  //this.map.infoWindow.show();
		  if(this.config.resetAddRemove && this.eSelectTypeVal != 'new'){
			this.onNewSelection();
		  }
		  html.setStyle(this.progressBar.domNode, 'display', 'none');
		  html.setStyle(this.progressBar2.domNode, 'display', 'none');
		  html.setStyle(this.progressBar3.domNode, 'display', 'none');
	  },
	  
      _initProgressBar: function () {
        this.progressBar = new ProgressBar({
          indeterminate: true
        }, this.progressbar);
        html.setStyle(this.progressBar.domNode, 'display', 'none');
		this.progressBar2 = new ProgressBar({
          indeterminate: true
        }, this.progressbar2);
        html.setStyle(this.progressBar2.domNode, 'display', 'none');
		this.progressBar3 = new ProgressBar({
          indeterminate: true
        }, this.progressbar3);
        html.setStyle(this.progressBar3.domNode, 'display', 'none');
      },

	  searchArea: function(){
		var areasAndLengthParams = new AreasAndLengthsParameters();
        areasAndLengthParams.lengthUnit = GeometryService.UNIT_STATUTE_MILE;
        areasAndLengthParams.areaUnit = GeometryService.UNIT_SQUARE_MILES;
		areasAndLengthParams.polygons = [this.graphicGeometry];
		areasAndLengthParams.calculationType = 'preserveShape';
        esriConfig.defaults.geometryService.areasAndLengths(areasAndLengthParams, lang.hitch(this, function (measurements) {
			if(!measurements.areas[0] || measurements.areas[0]<this.minArea){
				if(!measurements.areas[0]){
					if(this.config.drawLocDetails === "required"){
						this.slLocation = "NOTE: Geoprocessing area error - Please try again";
					    this._onSearchFinish("message");
					}else{
						this.search();
					}
				}else{
					this.slLocation = "NOTE: Please select an area of a least "+this.config.drawMinArea+" sq mile (current selected area = "+Math.round(measurements.areas[0]*1000)/1000 +" sq mile)";
					this._onSearchFinish("message");
				}
			}else{
				this.slLocation = "a "+ Math.round(measurements.areas[0]*10)/10 +" square mile area";
				if(this.config.drawLocDetailsCenter === "no"){
					this.search();
				}else{
					this.searchCenter();
				}
			}	
        })); 
	  },

	  searchCenter: function(){
		  var queryParams = new Query();
		  if (this.graphicGeometry.rings.length>1){
			  var xTent=this.graphicGeometry.getExtent();
			  queryParams.geometry = xTent.getCenter();
		  }else{
			  queryParams.geometry = this.graphicGeometry.getCentroid();
		  }
		  var fields = this.config.layers[this.mcdLayerIndex].fields.all.split(",");
          queryParams.returnGeometry = false;
		  queryParams.outSpatialReference = this.map.spatialReference;
          queryParams.outFields = fields;
          var queryTask = new QueryTask(this.config.layers[this.mcdLayerIndex].url);
          queryTask.execute(queryParams, lang.hitch(this, this._onMCDCenter),
            lang.hitch(this, this._onMCDCenterError));
	  },
	  
	  _onMCDCenter: function(results){
		  var resultFeatures = results.features;
		  if (results.features.length === 0) {
			  this.slLocation += " centered out of state";
		  }else{
			  this.slLocation += " centered in the "+resultFeatures[0].attributes.Location;
		  }
          this.search();
	  },
	  
	  _onMCDCenterError: function(){
		  if(this.config.drawLocDetailsCenter === "required"){
			  this.slLocation = "NOTE: Geoprocessing center error - Please try again";
			  this._onSearchFinish("message");
		  }else{
			  this.search();
		  }
	  },
	  
	  graphicAddRem: function(newGraphic){
		  if(!this.graphicGeometry){
			  this.slLocation = 'NOTE: Please make a "New" selection before removing';
			  this._onSearchFinish("message");
		  }else{
		      if(this.eSelectTypeVal === "add"){
                  esriConfig.defaults.geometryService.union([this.graphicGeometry, newGraphic], lang.hitch(this, function (evt) {
			          if(!evt){
				          this.slLocation = "NOTE: Add Geoprocessing add error - Please try again (Please report if persistent)";
				          this._onSearchFinish("message");
			          }else{
					      this.graphicGeometry = evt;
				          if(this.config.drawLocDetails === "no"){
						      this.search();
			              }else{
				              this.searchArea();
			              }
		              }
                  }));
		      }else{
                  esriConfig.defaults.geometryService.difference([this.graphicGeometry], newGraphic, lang.hitch(this, function (evt) {
			          if(!evt[0]){
				          this.slLocation = "NOTE: Remove Geoprocessing remove error - Please try again (Please report if persistent)";
				          this._onSearchFinish("message");
			          }else{
					      this.graphicGeometry = evt[0];
				          if(this.config.drawLocDetails === "no"){
						      this.search();
			              }else{
				              this.searchArea();
			              }
		              }
                  }));
		      }		  
		  }
	  },
	  
	  
	  onNewSelection: function(){
        html.replaceClass(this.aSelectType.iconNode, 'newSelIcon', 'removeSelIcon');
		html.replaceClass(this.bSelectType.iconNode, 'newSelIcon', 'removeSelIcon');
		html.replaceClass(this.cSelectType.iconNode, 'newSelIcon', 'removeSelIcon');
        html.replaceClass(this.aSelectType.iconNode, 'newSelIcon', 'addSelIcon');
		html.replaceClass(this.bSelectType.iconNode, 'newSelIcon', 'addSelIcon');
		html.replaceClass(this.cSelectType.iconNode, 'newSelIcon', 'addSelIcon');
        this.eSelectTypeVal = 'new';
		//alert(this.eSelectTypeVal);
      },

      onAddSelection: function(){
        html.replaceClass(this.aSelectType.iconNode, 'addSelIcon', 'newSelIcon');
		html.replaceClass(this.bSelectType.iconNode, 'addSelIcon', 'newSelIcon');
		html.replaceClass(this.cSelectType.iconNode, 'addSelIcon', 'newSelIcon');
        html.replaceClass(this.aSelectType.iconNode, 'addSelIcon', 'removeSelIcon');
		html.replaceClass(this.bSelectType.iconNode, 'addSelIcon', 'removeSelIcon');
		html.replaceClass(this.cSelectType.iconNode, 'addSelIcon', 'removeSelIcon');
        this.eSelectTypeVal = 'add';
		//alert(this.eSelectTypeVal);
      },

      onRemoveSelection: function(){
        html.replaceClass(this.aSelectType.iconNode, 'removeSelIcon', 'newSelIcon');
		html.replaceClass(this.bSelectType.iconNode, 'removeSelIcon', 'newSelIcon');
		html.replaceClass(this.cSelectType.iconNode, 'removeSelIcon', 'newSelIcon');
        html.replaceClass(this.aSelectType.iconNode, 'removeSelIcon', 'addSelIcon');
		html.replaceClass(this.bSelectType.iconNode, 'removeSelIcon', 'addSelIcon');
		html.replaceClass(this.cSelectType.iconNode, 'removeSelIcon', 'addSelIcon');
        this.eSelectTypeVal = 'rem';
		//alert(this.eSelectTypeVal);
      },
    });
  });
