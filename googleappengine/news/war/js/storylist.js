
Ext.onReady(function(){
  var store = new Ext.data.JsonStore({
    root: 'stories',
    totalProperty: 'totalCount',
    idProperty: 'storyId',
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
      header: 'Link',
      dataIndex: 'body'
    },{
      header: 'Description',
      dataIndex: 'title'
    }]
  });

  grid.render();
  store.load({});
});