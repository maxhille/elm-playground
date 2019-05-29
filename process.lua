-- Nodes will only be processed if one of these keys is present

node_keys = { }

-- Initialize Lua logic

function init_function()
end

-- Finalize Lua logic()
function exit_function()
end

-- Assign nodes to a layer, and set attributes, based on OSM tags

function node_function(node)
end

-- Similarly for ways

function way_function(way)
	local highway = way:Find("highway")
	local building = way:Find("building")
	local natural = way:Find("natural")
	if natural=="water" then
		way:Layer("water", true)
	end
end
