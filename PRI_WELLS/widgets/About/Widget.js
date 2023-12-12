///////////////////////////////////////////////////////////////////////////
// Dave Mechenich custom help (about)
///////////////////////////////////////////////////////////////////////////
define([
    'dojo/_base/declare',
    'jimu/BaseWidget',
	'dijit/_WidgetsInTemplateMixin',
	'dijit/layout/ContentPane',
    'dijit/layout/TabContainer',
	'jimu/utils',
	'dojo/_base/html',
	'dojo/_base/lang',
	'dojo/on'
  ],
  function (
    declare, BaseWidget, _WidgetsInTemplateMixin, ContentPane, TabContainer, jimuUtils, html, lang, on
  ) {
    return declare([BaseWidget, _WidgetsInTemplateMixin], {
	  selTab: null,

      postCreate: function () {
        this.inherited(arguments);
		this.aData.innerHTML = this.config.dataDate;
		this.aViewer.innerHTML = this.config.viewerVersion;
		this.aFramework.innerHTML = this.config.devFramework;
		this.aJS.innerHTML = this.config.jsAPI;
        //this._initTabContainer();
      },

	  
	  onOpen: function () {
		//alert("onOpen");
      },
	  
	  onClose: function () {
		//alert("onClose");
      },
	  
	  /*_initTabContainer: function () {
        //var initView = this.config.initialView || "Overview";
        //this.selTab = initView;
		//this.selTab = "Overview";
		
		this.tabContainer = new TabContainer({
            style: "height: 100%; width: 100%;",
			tabPosition: "left-h",
			doLayout: "false"
        }, this.tabHelp);
		
		var cp1 = new ContentPane({
           title: "Overview",
           content: this.tabNode1
        });
        this.tabContainer.addChild(cp1);
		var cp2 = new ContentPane({
           title: "Content",
           content: this.tabNode2
        });
        this.tabContainer.addChild(cp2);
		var cp3 = new ContentPane({
           title: "Display",
           content: this.tabNode3
        });
        this.tabContainer.addChild(cp3);
		var cp4 = new ContentPane({
           title: "Search/Locate",
           content: this.tabNode4
        });
        this.tabContainer.addChild(cp4);
		var cp5 = new ContentPane({
           title: "Navigation",
           content: this.tabNode5
        });
        this.tabContainer.addChild(cp5);
		var cp6 = new ContentPane({
           title: "Other Tools",
           content: this.tabNode6
        });
        this.tabContainer.addChild(cp6);
		var cp7 = new ContentPane({
           title: "Info",
           content: this.tabNode7
        });
        this.tabContainer.addChild(cp7);
		
		this.tabContainer.startup();
		
        //this.own(on(this.tabContainer, "tabChanged", lang.hitch(this, function (title) {
        //  this.selTab = title;
        //})));
      },*/
	});
  });
