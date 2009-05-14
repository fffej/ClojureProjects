// Load up some stories from a URL and display them in an Ext Grid

Ext.onReady(function(){
  var store = new Ext.data.JsonStore({
    root: 'stories',
    totalProperty: 'totalCount',
    idProperty: 'storyId',
    remoteSort: true,

    fields: [
      'body', 'title'
    ],

    proxy: new Ext.data.HttpProxy({
      url: 'http://localhost:8080/liststory?'
    })
  });

  var grid = new Ext.grid.GridPanel({
    el: 'story-grid',
    title: 'Clojure News!',
    store: store,
    loadMask: true,
    height: 400,
    columns:[{
      header: 'Title',
      dataIndex: 'body'
    },{
      header: 'Link',
      dataIndex: 'title'
    }]
  });

  grid.render();

  // trigger the datastore load
  store.load({});
});