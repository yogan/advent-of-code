local M = {}

local function in_bounds(garden, r, c)
	return r > 0 and c > 0 and r <= #garden and c <= #garden[1]
end

local function flood_fill(garden, r, c, seen)
	local char = garden[r]:sub(c, c)
	local region = {}
	local border = { { r, c } }

	while #border > 0 do
		local rc = table.remove(border, 1)
		r = rc[1]
		c = rc[2]

		if
			in_bounds(garden, r, c) --
			and not (seen[r] and seen[r][c])
			and garden[r]:sub(c, c) == char
		then
			seen[r] = seen[r] or {}
			seen[r][c] = true
			table.insert(region, { r, c })
			table.insert(border, { r - 1, c })
			table.insert(border, { r + 1, c })
			table.insert(border, { r, c - 1 })
			table.insert(border, { r, c + 1 })
		end
	end

	return region
end

local function perimeter(region, garden, r, c)
	local char = garden[r]:sub(c, c)
	local sides = {}
	local length = 0

	for _, rc in ipairs(region) do
		r = rc[1]
		c = rc[2]

		for _, rrccside in ipairs({
			{ "T", r - 1, c },
			{ "B", r + 1, c },
			{ "L", r, c - 1 },
			{ "R", r, c + 1 },
		}) do
			local side = rrccside[1]
			local rr = rrccside[2]
			local cc = rrccside[3]

			if
				not in_bounds(garden, rr, cc) --
				or garden[rr]:sub(cc, cc) ~= char
			then
				local key = side .. "," .. r .. "," .. c
				sides[key] = { side, r, c }
				length = length + 1
			end
		end
	end

	return { length, sides }
end

local function walk(tuple, dr, dc, sides, seen)
	local function key(d, r, c)
		return d .. "," .. r .. "," .. c
	end

	local d = tuple[1]
	local r = tuple[2]
	local c = tuple[3]
	local cc = c + dc
	local rr = r + dr
	local k = key(d, rr, cc)

	while sides[k] and not seen[k] do
		seen[k] = true
		cc = cc + dc
		rr = rr + dr
		k = key(d, rr, cc)
	end
end

local function count_sides(sides)
	local seen = {}
	local res = 0

	for k, tuple in pairs(sides) do
		if not seen[k] then
			seen[k] = true
			res = res + 1
			if tuple[1] == "T" or tuple[1] == "B" then
				walk(tuple, 0, -1, sides, seen)
				walk(tuple, 0, 01, sides, seen)
			else
				walk(tuple, -1, 0, sides, seen)
				walk(tuple, 01, 0, sides, seen)
			end
		end
	end

	return res
end

function M.regions(garden)
	local seen = {}
	local regions = {}

	for r = 1, #garden do
		for c = 1, #garden[1] do
			if not (seen[r] and seen[r][c]) then
				local region = flood_fill(garden, r, c, seen)
				local per = perimeter(region, garden, r, c)
				table.insert(regions, { #region, per[1], count_sides(per[2]) })
			end
		end
	end

	return regions
end

function M.parts(regions)
	local p1 = 0
	local p2 = 0
	for _, region in ipairs(regions) do
		p1 = p1 + region[1] * region[2]
		p2 = p2 + region[1] * region[3]
	end
	return { p1, p2 }
end

return M
