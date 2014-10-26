// *******************************************************
// expressjs template
//
// assumes: npm install express
// defaults to jade engine, install others as needed
//
// assumes these subfolders:
//   public/
//   public/javascripts/
//   public/stylesheets/
//   views/
//
var express = require('express');

var app = express();

var routes = require('./routes');
var handlers = require('./routes/index');
var config = require('./config');
var GrantSummary = require('./grantsummary').GrantSummary;
var fs = require('fs');
 
var expressLogFile = fs.createWriteStream('./logs/express.log', {flags: 'a'});
app.set('view engine', 'ejs');
app.set('title', 'VizMyGrants');



// Configuration
app.configure(function(){
  app.use(express.logger({stream: expressLogFile}));
  app.use(express.bodyParser());
  app.use(express.methodOverride());
  app.use(express.favicon("images/favicon.png")); 
  app.use(app.router);
  app.use(express.static(__dirname + '/public'));
});
app.configure('development', function(){
  app.use(express.errorHandler({ dumpExceptions: true, showStack: true }));
});
app.configure('production', function(){
  app.use(express.errorHandler());
});

var grantsummary = new GrantSummary(config.mongodb);

function start() {
  routes.setup(app, handlers, grantsummary);
  var port = process.env.PORT || 3000;
  app.listen(port);
  console.log("Express server listening on port %d in %s mode", port, app.settings.env);
}
// *******************************************************
exports.start = start;
exports.app = app;