function setup(app, handlers, grantsummary) {
  app.get('/', handlers.index);
  app.get('/index.html', handlers.index);
  app.post('/api/getd3data', function(req, res){
  	handlers.visualise(req, res, grantsummary);
  });

}
 
exports.setup = setup;