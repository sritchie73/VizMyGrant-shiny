var MongoClient = require('mongodb').MongoClient;

var connect = function(callback){
	MongoClient.connect('mongodb://localhost:27017/test', callback);
};

var buildAggregateQueryJSON = function(parameters) {
	console.log("parameters for query:" + JSON.stringify(parameters));
	var aggregateQuery = "{\"$group\":{\"_id\":{";
	var isFirst = true;
	var iterator = 1;
	parameters.level.forEach(function(level) {
		if(!isFirst) {
			aggregateQuery += ",";
		}
		aggregateQuery += "\"level"+iterator+"\":\"$"+level+"\"";
		if(isFirst) {
			isFirst = false;
		} 
		iterator += 1;
	});
	var figureToSum = (parameters.measure=="dollars")? "\"$TotalAmount\"" : "1";
	aggregateQuery += "},\"total\":{\"$sum\" : " + figureToSum + "}}}";
	console.log(aggregateQuery);
	return JSON.parse(aggregateQuery);
};

GrantSummary = function(){};

GrantSummary.prototype.generateHierarchy = function(parameters, callback) {
   connect(function(err, db) {
 	if(err) callback(err);
 	var query = buildAggregateQueryJSON(parameters);
    var cursor = db.collection('grantsummary').aggregate([
        query
      ], function(err, result) {
      	db.close();
      	callback(err, result)
      });
   })
};

exports.GrantSummary = GrantSummary;