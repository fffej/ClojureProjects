// Load up some stories from a URL and display them in an Ext Grid

Ext.onReady(function(){
  var store = new Ext.data.JsonStore({
    root: 'stories',
    totalProperty: 'totalCount',
    idProperty: 'storyId',
    remoteSort: true,

    fields: [
    ],

    proxy: new Ext.data.HttpProxy({
      url: 'http://localhost:8080/liststory'
    })
  });

  var grid = new Ext.grid.GridPanel({
    el: 'story-grid',
    title: 'Clojure News!',
    store: store,
    loadMask: true,
    columns:[{
      header: 'Title',
      dataIndex: 'storyTitle'
    },{
      header: 'Link',
      dataIndex: 'storyLink'
    }]
  });

  grid.render();

  // trigger the datastore load
  store.load({params: {maxresults: 25}});
});