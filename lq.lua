#!/usr/bin/lua

local table = require("table")

local mltostring = require("ml").tstring

--- Nicer printing for tables, everything else behaves the same as before
local function prettyprint(object)
  if type(object) == "table" then
    object = mltostring(object, {spacing="", indent="  "})
  end
  print(object)
end

--- Count all entries in a generic table
local function count(t)
	local length = 0
	for _,_ in pairs(t) do
		length = length + 1
	end
	return length
end

local lpeg = require("lpeg")

-- Use more obvious names
--local match = lpeg.match
local literal = lpeg.P
local grammar = lpeg.P
local variable = lpeg.V
--local set = lpeg.S
local range = lpeg.R
local capture = lpeg.C
--local capture_table = lpeg.Ct
--local capture_substitute = lpeg.Cs

-- Retrieve available character classes
local character_class = {}
lpeg.locale(character_class)

-- LPEG uses `1` to denote any character (`.` in PCRE)
local any = literal(1)

--[[
local function sequence(pattern1, pattern2)
  return pattern1 * pattern2
end
local function oneof(pattern1, pattern2)
  return pattern1 + pattern2
end
local function butnot(pattern1, pattern2)
  return pattern1 - pattern2
end
local function postprocess(capture, func)
  return capture / func
end
]]

--- Match a maximum of one (i.e. optional) instance of `p`
local function maybe(p)
  return p^-1
end
--- Match any number of instances of `p`
local function zeroormore(p)
  return p^0
end
--- Match at least one instance of `p`
local function oneormore(p)
  return p^1
end

--[[
--- Substitute the empty string '' for every occurance of `p`
local function discard(p)
  return capture(p)/''
end
]]

-- Matches
local optional_space = zeroormore(character_class.space)
local function withspace(pattern)
  return optional_space * pattern * optional_space
end
local function list(pattern)
  pattern = withspace(pattern)
  return pattern * zeroormore(',' * pattern)
end

local dot = literal(".")
local digit = range("09")
local digits = oneormore(digit)
local quote = literal("\"")
local string_literal = quote * capture(zeroormore(any - quote)) * quote
local all_literals = string_literal + digits/tonumber
local identifier_char = range("AZ", "az") + literal("_")
local identifier = identifier_char * oneormore(identifier_char + digit)
local field_selector = dot * capture(identifier)
local index_selector = maybe(dot) * literal("[") * withspace(all_literals) * literal("]")
local slice_selector = maybe(dot) * literal("[") * withspace(digits/tonumber) * literal(":") * withspace(digits/tonumber) * literal("]")
local value_selector = maybe(dot) * literal("[]")
local function_call = withspace(capture(identifier)) * maybe(literal("(") * maybe(list(all_literals)) * literal(")"))
local boolean_operator = withspace(capture(literal("and") + literal("or")))
local pipe = withspace(literal("|"))

-- Actions
local function forall(f)
  return function(inputs)
    local outputs = {}
    for _,input in ipairs(inputs) do
      local output = f(input)
      if output then
        table.insert(outputs, output)
      end
    end
    return outputs
  end
end

local function pass()
  return function(input)
    return input
  end
end

local function select_field(index)
  return function(input)
    return input[index]
  end
end
local select_field_forall = function(index) return forall(select_field(index)) end

local function slice_sequence(first, last)
  return function(input)
    return table.move(input, first, last, 1)
  end
end
local slice_sequence_forall = function(first, last) return forall(slice_sequence(first, last)) end

local function unwrap_forall()
  return function(inputs)
    local outputs = {}
    for _,input in ipairs(inputs) do
      for _,v in pairs(input) do
        table.insert(outputs, v)
      end
    end
    return outputs
  end
end

local function compose_expressions(expression1, expression2)
  return function(input)
    return expression2(expression1(input))
  end
end

local function keys_function()
  return function(input)
    local output = {}
    for k,_ in pairs(input) do
      table.insert(output, k)
    end
    return output
  end
end

local function values_function()
  return function(input)
    local output = {}
    for _,v in pairs(input) do
      table.insert(output, v)
    end
    return output
  end
end

local function len_function()
  return function(input)
    return #input
  end
end

local function length_function()
  return function(input)
    return count(input)
  end
end

local function has_function(index)
  return function(input)
    return input[index] ~= nil
  end
end

local function call_builtin_function(id, ...)
  if id == "keys" then
    assert(#{...} == 0, id .. "() takes no arguments")
    return forall(keys_function())
  elseif id == "values" then
    assert(#{...} == 0, id .. "() takes no arguments")
    return forall(values_function())
  elseif id == "len" then
    assert(#{...} == 0, id .. "() takes no arguments")
    return forall(len_function())
  elseif id == "length" then
    assert(#{...} == 0, id .. "() takes no arguments")
    return forall(length_function())
  elseif id == "has" then
    assert(#{...} == 1, id .. "() takes exactly one argument")
    return forall(has_function(...))
	elseif id == "error" then
		local message = ... or "Unknown error"
		return function() return error(message) end
  else
    return error("No such function: " .. id)
  end
end

local function notempty(t)
	if count(t) == 0 then
		return nil
	end
	return t
end

local function binary_operation(expression1, operator, expression2)
  if operator == "and" then
    return function(input)
			return notempty(expression1(input)) and notempty(expression2(input))
		end
  elseif operator == "or" then
    return function(input)
			return notempty(expression1(input)) or notempty(expression2(input))
		end
  else
    return error("No such binary operator: " .. operator)
  end
end

local function parse_error(...)
  return error("Parse error at: " .. mltostring(...))
end

-- All selectors which can be chained and their actions
local chainable_selector = field_selector/select_field_forall + index_selector/select_field_forall + slice_selector/slice_sequence_forall + value_selector/unwrap_forall

-- Grammar which builds the filter function
local lq = grammar{
	variable("Expression_Composition") * (oneormore(any)/parse_error) + variable("Expression_Composition"),
	Expression_Composition = (variable("Expression") * pipe * variable("Expression_Composition"))/compose_expressions + variable("Expression"),
  Expression = variable("Binary_Expression") + variable("Unary_Expression"),
  Binary_Expression = (variable("Unary_Expression") * boolean_operator * variable("Binary_Expression"))/binary_operation + variable("Unary_Expression"),
  Unary_Expression = function_call/call_builtin_function + variable("Selector") + dot/pass,
  Selector = (chainable_selector * variable("Selector"))/compose_expressions + chainable_selector,
}

-- main()
local selector = arg[1]
assert(selector)

-- Parse the selector and create a filter function
local filter = lq:match(selector)

local inputs = {}

-- If no file argument was given, read from stdin
if #arg == 1 then
  inputs = {dofile()}
end

-- Otherwise read all given files as distinct inputs
for i=2,#arg do
  local filename = arg[i]
  local input = dofile(filename)
  table.insert(inputs, i-1, input)
end

local outputs = filter(inputs)
forall(prettyprint)(outputs)
