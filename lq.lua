#!/usr/bin/lua

local selector = arg[1]
local filename = arg[2]

local data = dofile(filename)
local select = assert(load("local data = ...; return data" .. (selector or "")))
local result = select(data)
if type(result) == "table" then
	for k,_ in pairs(result) do
		print(k)
	end
else
	print(result)
end
