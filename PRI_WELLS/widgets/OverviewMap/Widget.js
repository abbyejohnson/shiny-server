///////////////////////////////////////////////////////////////////////////
// Copyright © 2014 Esri. All Rights Reserved.
//
// Licensed under the Apache License Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//    http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
///////////////////////////////////////////////////////////////////////////

define([
    'dojo/_base/declare',
    'dojo/_base/lang',
    'dojo/_base/html',
    'dojo/_base/array',
    'dojo/on',
    'dojo/aspect',
    'jimu/BaseWidget',
    'esri/dijit/OverviewMap',
	'esri/layers/ArcGISDynamicMapServiceLayer',
    'jimu/utils',
    "dojo/dom-style"
  ],
  function(
    declare,
    lang,
    html,
    array,
    on,
    aspect,
    BaseWidget,
    OverviewMap,
	ArcGISDynamicMapServiceLayer,
    utils,
    domStyle) {
    var clazz = declare([BaseWidget], {

      baseClass: 'jimu-widget-overview',
      overviewMapDijit: null,
      _showDijit: false,
      _handles: null,
	  xFactor: null,
	  xFIndex: 1,
	  xFMult: 2,
	  omLayer: null,

      startup: function() {
		this.xFactor = this.config.xFactor.split(",").map(Number);
		this.xFMult = this.config.xFMult;
		
        this._handles = [];
        this.inherited(arguments);
		
		this.omLayer = new ArcGISDynamicMapServiceLayer(this.config.url, {
		    //"http://services.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer", {
		    //"https://gissrv2.uwsp.edu/ArcGIS/rest/services/Pri_Wells_Canvas/MapServer", {
            "showAttribution": false
        });
		
		if(this.omLayer.loaded){
           this.whenBMLoaded();
        } else {
			this.own(this.omLayer.on("load", lang.hitch(this, this.whenBMLoaded())));
        }
      },

	  whenBMLoaded: function(visible) {
		//alert("whenLoaded");
		this.createOverviewMap(visible);
		this._handles.push(aspect.before(
            this.overviewMapDijit,
            'show',
            lang.hitch(this, '_beforeOverviewShow')
        ));
        //if (this.map) {
        //  this.own(on(this.map, 'layer-add', lang.hitch(this, this._onMainMapBasemapChange)));
          //this.own(on(this.map, 'resize', lang.hitch(this, this._onMainMapResize)));
        //}
	  },
	  
      setPosition: function(position) {
        this.position = position;
        html.place(this.domNode, this.map.id);
        this._processAttachTo(this.config, position);
        if (this.started) {
          this._updateDomPosition(this.config.attachTo);
        }
      },

      _processAttachTo: function(config, position) {
        if (!config.attachTo && position) {
          if (position.top !== undefined && position.left !== undefined) {
            config.attachTo = !window.isRTL ? "top-left" : "top-right";
          } else if (position.top !== undefined && position.right !== undefined) {
            config.attachTo = !window.isRTL ? "top-right" : "top-left";
          } else if (position.bottom !== undefined && position.left !== undefined) {
            config.attachTo = !window.isRTL ? "bottom-left" : "bottom-right";
          } else if (position.bottom !== undefined && position.right !== undefined) {
            config.attachTo = !window.isRTL ? "bottom-right" : "bottom-left";
          }
        }
      },

      _updateDomPosition: function(attachTo) {
        if (this.overviewMapDijit) {
          var initPos = {
            left: 'auto',
            right: 'auto',
            top: 'auto',
            bottom: 'auto',
            width: 'auto',
            zIndex: (this.position && this.position.zIndex) || 0
          };
          var _position = this._getOverviewPositionByAttach(attachTo);
          lang.mixin(initPos, _position);
          var style = utils.getPositionStyle(initPos);
          style.position = 'absolute';
          domStyle.set(this.domNode, style);
          domStyle.set(this.overviewMapDijit.domNode, style);
        }
      },

      createOverviewMap: function(visible) {
        var json = lang.clone(this.config.overviewMap);
        json.map = this.map;
        if (visible !== undefined) {
          json.visible = visible;
        }
        this._processAttachTo(json, this.position);

        // overviewMap dijit has bug in IE8
        var _isShow = json.visible;
        json.visible = false;
        var _hasMaximizeButton = 'maximizeButton' in json;
        json.maximizeButton = _hasMaximizeButton ? json.maximizeButton : true;
        json.width = 200;
        json.height = 200;
		json.color = 'red';
        json.expandFactor = this.xFactor[this.xFIndex]*this.xFMult;
		json.baseLayer = this.omLayer;
        this.overviewMapDijit = new OverviewMap(json);
        
        this._handles.push(aspect.after(
          this.overviewMapDijit,
          'show',
          lang.hitch(this, '_afterOverviewShow')
        ));
        this._handles.push(aspect.after(
          this.overviewMapDijit,
          'hide',
          lang.hitch(this, '_afterOverviewHide')
        ));
        this.overviewMapDijit.startup();

        this._updateDomPosition(json.attachTo);
        this.domNode.appendChild(this.overviewMapDijit.domNode);
        if (_isShow) {
          this.overviewMapDijit.show();
        }
      },

      _getOverviewPositionByAttach: function(attachTo) {
        var _position = {};
        if (attachTo === 'top-left') {
          _position.left = 0;
          _position.top = 0;
        } else if (attachTo === 'top-right') {
          _position.right = 0;
          _position.top = 0;
        } else if (attachTo === 'bottom-left') {
          _position.bottom = 0;
          _position.left = 0;
        } else if (attachTo === 'bottom-right') {
          _position.bottom = 0;
          _position.right = 0;
        }

        if (window.isRTL) {
          if (isFinite(_position.left)) {
            _position.right = _position.left;
            delete _position.left;
          } else {
            _position.left = _position.right;
            delete _position.right;
          }
        }

        return _position;
      },

      /*_onMainMapBasemapChange: function(evt) {
        if (!(evt.layer && evt.layer._basemapGalleryLayerType)) {
          return;
        }

        this._destroyOverviewMap();
        this.createOverviewMap(this._showDijit);
      },*/

      onPositionChange: function(position) {
        this.position = position;
        if (this.config.overviewMap.attachTo) {
          return;
        }

        this._destroyOverviewMap();
        this.createOverviewMap(this._showDijit);
      },

      _destroyOverviewMap: function() {
		//alert("destroy");
        array.forEach(this._handles, function(handle) {
          if (handle && typeof handle.remove === 'function') {
            handle.remove();
          }
        });
        if (this.overviewMapDijit && this.overviewMapDijit.destroy) {
          this.overviewMapDijit.destroy();
          this.overviewMapDijit = null;
          html.empty(this.domNode);
        }
      },

      //_onMainMapResize: function() {
      //  this._destroyOverviewMap();  //  this.createOverviewMap(this._showDijit);
      //},

      onReceiveData: function(name) {
        if (name !== "BasemapGallery") {
          return;
        }

        this._destroyOverviewMap();
        this.createOverviewMap(this._showDijit);
      },

      onOpen: function() {
		//alert("onOpen");      
        //this._destroyOverviewMap();
        //this.createOverviewMap(this._showDijit);
      },

      onClose: function() {
        this._destroyOverviewMap();
      },

      _afterOverviewHide: function() {
        this._showDijit = false;
        domStyle.set(this.domNode, {
          width: "auto",
          height: "auto"
        });
      },
	  
	  _beforeOverviewShow: function() {
		//**add change expand factor
		var xLOD = this.map.getLevel();
		if (xLOD !== this.xFIndex){
			this.xFIndex = xLOD;
			this._destroyOverviewMap();
			this.omLayer = new ArcGISDynamicMapServiceLayer(this.config.url, {
		        //"http://services.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer", {
		        //"https://gissrv2.uwsp.edu/ArcGIS/rest/services/Pri_Wells_Canvas/MapServer", {
                "showAttribution": false
            });
			this.own(this.omLayer.on("load", lang.hitch(this, this.whenBMLoaded(true))));
		    //this.createOverviewMap(true);
		    //this._handles.push(aspect.before(
            //  this.overviewMapDijit,
            //  'show',
            //  lang.hitch(this, '_beforeOverviewShow')
            //));
		}
      },

      _afterOverviewShow: function() {
		//alert(this.overviewMapDijit.color);
        this._showDijit = true;
        domStyle.set(this.domNode, {
          width: this.overviewMapDijit.width + 'px',
          height: this.overviewMapDijit.height + 'px'
        });
      }
    });

    return clazz;
  });