var fs = require('fs');

exports.index = function(req, res){
    res.render('index');
};

exports.circles = function(req, res){
    res.render('circles');
};

exports.topography = function(req, res){
    res.render('topography');
};

exports.visualise = function(req, res, grantsummary){
    var parameters = req.body;
    grantsummary.generateHierarchy(parameters, function(err, docs){
        if(err) throw err;
        res.json(docs);
    });
};

