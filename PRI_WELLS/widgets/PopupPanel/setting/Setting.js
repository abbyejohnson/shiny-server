define([
    'dojo/_base/declare',
    'dijit/_WidgetsInTemplateMixin',
    'jimu/BaseWidgetSetting',
    'jimu/dijit/Message',
    'jimu/dijit/CheckBox'
  ],
  function(
    declare,
    _WidgetsInTemplateMixin,
    BaseWidgetSetting,
    Message) {
    return declare([BaseWidgetSetting, _WidgetsInTemplateMixin], {
      //these two properties is defined in the BaseWidget
      baseClass: 'widget-popuppanel-setting',

      startup: function() {
        this.inherited(arguments);
        this.setConfig(this.config);
      },

      setConfig: function(config) {
        this.config = config;
        if (!config.closeOnClear) {
          this.closeonclear.setValue(false);
        } else {
          this.closeonclear.setValue(true);
        }
        if (!config.closeAtStart) {
          this.closeonstart.setValue(false);
        } else {
          this.closeonstart.setValue(true);
        }
      },

      getConfig: function() {
        this.config.closeAtStart = this.closeonstart.checked;
        this.config.closeOnClear = this.closeonclear.checked;
        return this.config;
      }

    });
  });
