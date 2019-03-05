var VectorTile = require('@mapbox/vector-tile').VectorTile;
var Protobuf = require('pbf');
var app = Elm.Main.init();

app.ports.parseMbv.subscribe(function(str) {
	var u8 = new TextEncoder("utf-8").encode(str);
	var pb = new Protobuf(u8);
	console.log(pb);
	var tile = new VectorTile(pb);
});

// Contains a map of all layers
//tile.layers;

//var landuse = tile.layers.landuse;

// Amount of features in this layer
//landuse.length;

// Returns the first feature
//landuse.feature(0);
//var elm = require("elm");
