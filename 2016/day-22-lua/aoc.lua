local M = {}

function M.parse(lines)
	local max_x, max_y = 0, 0
	local nodes = {}

	for _, line in ipairs(lines) do
		if line:match("^/dev") then
			local pattern = "node%-x(%d+)%-y(%d+)%s+(%d+)T%s+(%d+)T%s+(%d+)T%s+%d+%%"
			for x_str, y_str, size, used, avail in line:gmatch(pattern) do
				local x, y = tonumber(x_str), tonumber(y_str)

				if x > max_x then
					---@diagnostic disable-next-line: cast-local-type
					max_x = x
				end

				if y > max_y then
					---@diagnostic disable-next-line: cast-local-type
					max_y = y
				end

				if not nodes[x] then
					nodes[x] = {}
				end

				nodes[x][y] = {
					size = tonumber(size),
					used = tonumber(used),
					avail = tonumber(avail),
				}
			end
		end
	end

	return { nodes = nodes, max_x = max_x, max_y = max_y }
end

local function find_pairs(cluster)
	local pairs = {}
	for i = 0, cluster.max_x do
		for j = 0, cluster.max_y do
			local src = cluster.nodes[i][j]
			for k = 0, cluster.max_x do
				for l = 0, cluster.max_y do
					local tgt = cluster.nodes[k][l]
					if i ~= k or j ~= l then
						if src.used > 0 and src.used <= tgt.avail then
							table.insert(pairs, { src = { x = i, y = j }, tgt = { x = k, y = l } })
						end
					end
				end
			end
		end
	end
	return pairs
end

local function manhattan_distance(src, tgt)
	return math.abs(src.x - tgt.x) + math.abs(src.y - tgt.y)
end

local function can_move(nodes, src, tgt)
	assert(manhattan_distance(src, tgt) == 1)
	return nodes[src.x][src.y].used <= nodes[tgt.x][tgt.y].avail
end

local function move(nodes, src, tgt)
	local s = nodes[src.x][src.y]
	local t = nodes[tgt.x][tgt.y]
	t.used = t.used + s.used
	t.avail = t.avail - s.used
	s.used = 0
	s.avail = s.size
end

function M.part1(cluster)
	return #find_pairs(cluster)
end

function M.part2(cluster)
	local nodes = cluster.nodes
	local steps = 0
	local pos = { x = cluster.max_x, y = 0 }

	-- simplified try: always move data left towards goal
	while pos.x > 0 do
		print(string.format("pos: (%d/%d), steps: %d", pos.x, pos.y, steps))
		local next_pos = { x = pos.x - 1, y = pos.y }

		if can_move(nodes, pos, next_pos) then
			move(nodes, pos, next_pos)
			pos = next_pos
			steps = steps + 1
			print(string.format("moved to (%d/%d)", pos.x, pos.y))
		else
			-- TODO
			print(string.format("Can't move to (%d/%d)", next_pos.x, next_pos.y))
			-- find a place for the data at next_pos
			local pairs = find_pairs(cluster)
		end

		steps = steps + 1
	end

	return steps
end

return M
