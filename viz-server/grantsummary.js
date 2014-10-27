var MongoClient = require('mongodb').MongoClient;

_ = function(config){
	
	var connectionstring = config.connectionstring;
	console.log("MongoDB:" + connectionstring);

	var _self = this;

	_self.connect = function(callback){
		MongoClient.connect(connectionstring, callback);
	};

	_self.buildAggregateQueryJSON = function(parameters) {
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

	_self.generateHierarchy = function(parameters, callback) {
	   _self.connect(function(err, db) {
	 	if(err) callback(err);
	 	var query = _self.buildAggregateQueryJSON(parameters);
	    var cursor = db.collection('grantsummary').aggregate([
	        query
	      ], function(err, result) {
	      	db.close();
	      	callback(err, result)
	      });
	   })
	};
};


exports.GrantSummary = _;