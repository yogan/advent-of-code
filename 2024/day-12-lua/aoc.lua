local M = {}

local function flood_fill(garden, r, c, rows, cols, seen)
	local char = garden[r]:sub(c, c)
	local region = {}
	local border = { { r, c } }

	while #border > 0 do
		local rc = table.remove(border, 1)
		r = rc[1]
		c = rc[2]

		if not (r < 1 or r > rows or c < 1 or c > cols or (seen[r] and seen[r][c]) or garden[r]:sub(c, c) ~= char) then
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

local function perimeter(region, garden, r, c, rows, cols)
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

			if rr < 1 or rr > rows or cc < 1 or cc > cols or garden[rr]:sub(cc, cc) ~= char then
				local key = side .. "," .. r .. "," .. c
				sides[key] = { side, r, c }
				length = length + 1
			end
		end
	end

	return { sides, length }
end

local function key(d, r, c)
	return d .. "," .. r .. "," .. c
end

local function count_sides(sides)
	local res = 0
	local seen = {}

	for k, tuple in pairs(sides) do
		if not seen[k] then
			res = res + 1
			seen[k] = true

			local d = tuple[1]
			local r = tuple[2]
			local c = tuple[3]

			if d == "T" or d == "B" then
				local cc = c - 1
				k = key(d, r, cc)
				while sides[k] and not seen[k] do
					seen[k] = true
					cc = cc - 1
					k = key(d, r, cc)
				end

				cc = c + 1
				k = key(d, r, cc)
				while sides[k] and not seen[k] do
					seen[k] = true
					cc = cc + 1
					k = key(d, r, cc)
				end
			else
				local rr = r - 1
				k = key(d, rr, c)
				while sides[k] and not seen[k] do
					seen[k] = true
					rr = rr - 1
					k = key(d, rr, c)
				end

				rr = r + 1
				k = key(d, rr, c)
				while sides[k] and not seen[k] do
					seen[k] = true
					rr = rr + 1
					k = key(d, rr, c)
				end
			end
		end
	end

	return res
end

function M.regions(garden)
	local rows = #garden
	local cols = #garden[1]
	local seen = {}
	local regions = {}

	for r = 1, rows do
		for c = 1, cols do
			if not (seen[r] and seen[r][c]) then
				local region = flood_fill(garden, r, c, rows, cols, seen)
				local res = perimeter(region, garden, r, c, rows, cols)
				local sides = res[1]
				local perim = res[2]
				table.insert(regions, { #region, perim, count_sides(sides) })
			end
		end
	end

	return regions
end

function M.part1(regions)
	local sum = 0
	for _, region in ipairs(regions) do
		sum = sum + region[1] * region[2]
	end
	return sum
end

function M.part2(regions)
	local sum = 0
	for _, region in ipairs(regions) do
		sum = sum + region[1] * region[3]
	end
	return sum
end

return M
