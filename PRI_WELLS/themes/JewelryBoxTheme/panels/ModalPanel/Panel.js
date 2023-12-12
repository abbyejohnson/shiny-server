///////////////////////////////////////////////////////////////////////////
// this is the create a modal panel example from ESRI
///////////////////////////////////////////////////////////////////////////
define(['dojo/_base/declare',
  'jimu/BaseWidgetPanel',
  'dijit/_TemplatedMixin',
  //'dojo/on',
  'dojo/text!./Panel.html',
],
function(declare, BaseWidgetPanel), _TemplatedMixin, template) {

  return declare([BaseWidgetPanel, _TemplatedMixin], {
    templateString: template,
    baseClass: 'jimu-panel jimu-modal-panel',
	
	*DEFAULT_POSITION: {
      width: 450,
      height: 400
    },

    postCreate: function() {
      this.inherited(arguments);
    },

    startup: function() {
      this.inherited(arguments);
	  this._setPosition();
    },
	
	_setPosition: function() {
      var positionConfig = this.config.panel.position;
      if (!positionConfig) return;

      // use default width if no width is set
      if (!('width' in positionConfig)) {
        this.domNode.style.width = this.DEFAULT_POSITION.width + 'px';
      }
      // use default height if no height is set
      if (!('height' in positionConfig)) {
        this.domNode.style.height = this.DEFAULT_POSITION.height + 'px';
      }
      // vertically position the panel in the center
      // if no left and right properties are set
      if (!('left' in positionConfig || 'right' in positionConfig)) {
        this.domNode.style.left = '50%';
        this.domNode.style.marginLeft = -this.DEFAULT_POSITION.width * 0.5 + 'px';
      }
      // vertically position the panel in the center
      // if no top and bottom properties are set
      if (!('top' in positionConfig || 'bottom' in positionConfig)) {
        this.domNode.style.top = '50%';
        this.domNode.style.marginTop = -this.DEFAULT_POSITION.height * 0.5 + 'px';
      }
    }
	
	/*_initTitlePane: function() {
      var appConfig = this.config;
      if(!appConfig) return;

      // add icon
      if(appConfig.icon) {
        this.titleIconNode.src = appConfig.icon;
        this.titleIconNode.alt = appConfig.icon;
      } else {
        this.titleNode.removeChild(this.titleIconNode.parentNode);
      }
      // add label
      if(appConfig.label) {
        this.titleLabelNode.innerHTML = appConfig.label;
      } else {
        this.titleNode.removeChild(this.titleLabelNode);
      }
	  // close button event handler
      var self = this;
      this.own(on(this.closeButtonNode, 'click', function() {
        self.panelManager.closePanel(self);
      }));
    },
	
	_toggleOverlay: function(isShow) {
      // create overlay
      if(!this.overlayNode) {
        this.overlayNode = document.createElement('DIV');
        this.overlayNode.className = 'jimu-modal-panel-overlay hidden';
        if(this.domNode.parentNode) {
          this.domNode.parentNode.insertBefore(this.overlayNode, this.domNode);
        }
      }
      // add/remove class 'hidden' to/from the overlay
      if(isShow) {
        domClass.remove(this.overlayNode, 'hidden');
      } else {
        domClass.add(this.overlayNode, 'hidden');
      }
    },
	
	onOpen: function() {
      this.inherited(arguments);
      this._toggleOverlay(true);
	  this._repositionContent();
    },
	
	onClose: function() {
      this.inherited(arguments);
      this._toggleOverlay(false);
    },
	
	_repositionContent: function() {
      var newTop = this.titleNode.clientHeight;
      this.containerNode.style.top = newTop + 'px';
    }*/
	
  });
});