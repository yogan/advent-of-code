local M = {}

function M.parse(lines)
	local nodes = {}
	for _, line in ipairs(lines) do
		if line:match("^/dev") then
			local pattern = "node%-x(%d+)%-y(%d+)%s+(%d+)T%s+(%d+)T%s+(%d+)T%s+(%d+)%%"
			for x, y, size, used, avail, use in line:gmatch(pattern) do
				table.insert(nodes, {
					x = tonumber(x),
					y = tonumber(y),
					size = tonumber(size),
					used = tonumber(used),
					avail = tonumber(avail),
					use = tonumber(use),
				})
			end
		end
	end
	return nodes
end

function M.part1(nodes)
	local viable_pairs = 0
	for i, node_a in ipairs(nodes) do
		for j, node_b in ipairs(nodes) do
			if i ~= j and node_a.used > 0 and node_a.used <= node_b.avail then
				viable_pairs = viable_pairs + 1
			end
		end
	end
	return viable_pairs
end

return M
