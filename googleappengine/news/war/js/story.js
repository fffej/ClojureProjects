Ext.onReady(function() {
  var simple = new Ext.form.FormPanel({
        standardSubmit: true,
        frame:true,
        title: 'Submit New Story',
        width: 600,
	height: 400,
        defaults: {width: 600},
        defaultType: 'textfield',
	items: [{
	  fieldLabel: 'Story URL',
	  name: 'storyLink',
          allowBlank:false
        },
	{
	  fieldLabel: 'Story Title',
	  name: 'storyTitle',
	  allowBlank: false
	},
	{
	  inputType: 'hidden',
          id: 'submitbutton',
          name: 'hiddenbutton',
          value: 'hiddenvalue'
        }],
        buttons: [{
            text: 'Submit',
            handler: function() {
		simple.getForm().getEl().dom.action = '/savestory';
	        simple.getForm().getEl().dom.method = 'GET';
                simple.getForm().submit();
            }
        }]
    });



    simple.render('submitstory');
});