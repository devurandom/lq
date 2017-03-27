#!/usr/bin/lua5.3

--[[
Concepts:
* value
* input := value
* outputs := {value...}
* filter(input) -> outputs

Evaluation happens in two stages:
1. Compilation:
  - The selector argument to `lq` is parsed, producing a filter(input) function
  - The global filter function itself calls operators and filter functions, one for each stage of the selector pipeline
2. Evaluation:
  - Input is always literal, i.e. it is never a filter function
  - Operators see all inputs and call their arguments (filters) on each
  - Every filter is a function of its input that is evaluated at runtime
  - Filter functions take their arguments literally - `call()` ensures this
  - Each is evaluated for each available input, i.e. it never sees more than one input at a time
  - It may produce an arbitrary number of outputs
]]

local argparse = require "argparse"

local values = require("std.table").values
local keys = require("std.table").keys

local size = require("std.table").size

local bind = require("std.functional").bind

local insert = require("std.table").insert
local remove = require "std.table".remove
local move = require("table").move
local unpack = require("table").unpack

local sub = require("string").sub

local mltostring = require("ml").tstring

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

local dot = literal(".")
local colon = literal(":")
local digit = range("09")
local digits = oneormore(digit)
local quote = literal("\"")
local lparen = literal("(")
local rparen = literal(")")
local lbracket = literal("[")
local rbracket = literal("]")
local keyword_if = literal("if")
local keyword_then = literal("then")
local keyword_else = literal("else")
local keyword_end = literal("end")

local number_literal = (maybe(literal("+") + literal("-")) * digits)/tonumber
local string_literal = quote * capture(zeroormore(any - quote)) * quote
local all_literals = string_literal + number_literal
local identifier_char = range("AZ", "az") + literal("_")
local identifier = capture(identifier_char * zeroormore(identifier_char + digit) * maybe(literal("?")))

-- Helper for certain functions, which expect exactly one argument
local function ensure_single_result(filter, input)
  local outputs = filter(input)
  assert(#outputs <= 1, "Expected max. one result, got " .. #outputs .. ": " .. mltostring(outputs))
  return outputs[1]
end

--- Nicer printing for tables, everything else behaves the same as before
local function prettyprint(object)
  if type(object) == "table" then
    object = mltostring(object, {spacing="", indent="  "})
  end
  print(object)
end

-- Filters
local function pass()
  return function(input)
    return {input}
  end
end

local function feed_literal(value)
	return function()
		return {value}
	end
end

local function feed_nil()
  return function()
    return {nil}
  end
end

local filters = {
  ["list"] = function(...)
    local arguments = {...}
    return function()
      return {arguments}
    end
  end,
  ["index"] = function(index)
    return function(input)
      assert(index, "Argument missing: index(*index*)")
      if type(index) == "string" and sub(index, #index) == "?" then -- FIXME: Implement this as unary operator
        if not input then
          return nil
        end

        index = sub(index, 1, #index - 1)
      end
      return {input[index]}
    end
  end,
  ["slice"] = function(first, last)
    return function(input)
      assert(first, "Argument missing: slice(*first*, last)")
      assert(last, "Argument missing: slice(first, *last*)")
      return {move(input, first, last, 1, {})}
    end
  end,
  ["keys"] = function()
    return function(input)
      return keys(input)
    end
  end,
  ["values"] = function()
    return function(input)
      return values(input)
    end
  end,
  ["len"] = function()
    return function(input)
      return {#input}
    end
  end,
  ["length"] = function()
    return function(input)
      return {size(input)}
    end
  end,
  ["has"] = function(index)
    return function(input)
      assert(index, "Argument missing: has(*index*)")
      return {input[index] ~= nil}
    end
  end,
  ["not"] = function()
    return function(input)
      return {input == nil or input == false}
    end
  end,
  ["filter"] = function(condition)
    return function(input)
      if condition then
        return {input}
      else
        return {}
      end
    end
  end,
}

local function call(name, ...)
  local fn = assert(filters[name], "No such function: " .. name)
  local filter_arguments = {...}
  return function(input)
    local arguments = {}
    for _,filter in ipairs(filter_arguments) do
      local outputs = filter(input)
      move(outputs, 1, #outputs, #arguments + 1, arguments)
    end
    return fn(unpack(arguments))(input)
  end
end

-- Operators
local binary_operators = {
  ["|"] = function(filter1, filter2)
    return function(input)
      local filter1_outputs = filter1(input)
      if #filter1_outputs == 0 then
        return {filter2(nil)} -- Ensure chained filters are executed at least once. Necessary for `not()` to work.
      end

      local outputs = {}
      for _,intermediate in ipairs(filter1_outputs) do
        local filter2_outputs = filter2(intermediate)
        move(filter2_outputs, 1, #filter2_outputs, #outputs + 1, outputs)
      end
      return outputs
    end
  end,
  [","] = function(filter1, filter2)
    return function(input)
      local filter1_outputs = filter1(input)
      local filter2_outputs = filter2(input)
      return move(filter2_outputs, 1, #filter2_outputs, #filter1_outputs + 1, filter1_outputs)
    end
  end,
  ["=="] = function(filter1, filter2)
    return function(input)
      local filter1_output = ensure_single_result(filter1, input)
      local filter2_output = ensure_single_result(filter2, input)
      return {filter1_output == filter2_output}
    end
  end,
  ["!="] = function(filter1, filter2)
    return function(input)
      local filter1_output = ensure_single_result(filter1, input)
      local filter2_output = ensure_single_result(filter2, input)
      return {filter1_output ~= filter2_output}
    end
  end,
  ["and"] = function(filter1, filter2)
    return function(input)
      local filter1_output = ensure_single_result(filter1, input)
      local filter2_output = ensure_single_result(filter2, input)
      return {filter1_output and filter2_output}
    end
  end,
  ["or"] = function(filter1, filter2)
    return function(input)
      local filter1_output = ensure_single_result(filter1, input)
      local filter2_output = ensure_single_result(filter2, input)
      return {filter1_output or filter2_output}
    end
  end,
}

local function binary_operation(filter1, operator, filter2)
  local op = assert(binary_operators[operator], "No such binary operator: " .. operator)
  return op(filter1, filter2)
end

local function if_then_else(filter_condition, filter_true, filter_false)
  return function(input)
    local condition_output = filter_condition(input)
    if #condition_output > 0 and condition_output[1] then -- nil (i.e. empty output) and false ("false" in output) count as false
      return filter_true(input)
    elseif filter_false then
      return filter_false(input)
    end
    return {}
  end
end

local function parse_error(...)
  return error("Parse error at: " .. mltostring(...))
end

-- Grammar which builds the filter function
local lq = grammar{
	variable("Expression") * (oneormore(any)/parse_error)
    + variable("Expression"),
  Expression =
    (keyword_if * variable("Expression") * keyword_then * variable("Expression") * maybe(keyword_else * variable("Expression")) * keyword_end)/if_then_else
    + variable("Binary_Expression_Level1"),

  Binary_Expression_Level1 = (variable("Binary_Expression_Level2") * withspace(capture(literal("|"))) * variable("Binary_Expression_Level1"))/binary_operation
    + variable("Binary_Expression_Level2"),
  Binary_Expression_Level2 = (variable("Binary_Expression_Level3") * withspace(capture(literal("or"))) * variable("Binary_Expression_Level2"))/binary_operation
    + variable("Binary_Expression_Level3"),
  Binary_Expression_Level3 = (variable("Binary_Expression_Level4") * withspace(capture(literal("and"))) * variable("Binary_Expression_Level3"))/binary_operation
    + variable("Binary_Expression_Level4"),
  Binary_Expression_Level4 = (variable("Binary_Expression_Level5") * withspace(capture(literal("==") + literal("!="))) * variable("Binary_Expression_Level4"))/binary_operation
    + variable("Binary_Expression_Level5"),
  Binary_Expression_Level5 = (variable("Filter_Expression") * withspace(capture(literal(","))) * variable("Binary_Expression_Level5"))/binary_operation
    + variable("Filter_Expression"),

  Filter_Expression =
    withspace(variable("Spaced_Filter_Expression")),
  Spaced_Filter_Expression =
    variable("Selector_Chain")
    + (lparen * rparen)/feed_nil
    + (lparen * variable("Expression") * rparen)
    -- We need explicit parens here, since otherwise LPEG will eagerly right-reduce "binary" expressions, e.g. in `index (1)|has(2)`
    + (identifier * maybe(lparen * variable("Expression") * rparen))/call
    + (lbracket * rbracket)/feed_nil/bind(call, {"list"})
    + (lbracket * variable("Expression") * rbracket)/bind(call, {"list"})
    + all_literals/feed_literal
    + dot/pass,
  Selector_Chain =
    (variable("Dot_Selector") * variable("Selector_Chain"))/bind(binary_operation, {[2] = "|"})
    + variable("Dot_Selector"),
  Dot_Selector =
    dot * variable("Selector"),
  Selector =
    (lbracket * rbracket)/feed_nil/bind(call, {"values"})
    + (lbracket * variable("Expression") * colon * variable("Expression") * rbracket)/bind(call, {"slice"})
    + (lbracket * variable("Expression") * rbracket)/bind(call, {"index"})
    + (identifier/feed_literal)/bind(call, {"index"})
}

local inputs = {}

local function safe_dofile(filename)
  if filename == "STDIN" then
    filename = nil
  end
  local results = {pcall(dofile, filename)}
  local success = remove(results, 1)
  if not success then
    return nil, results[1]
  end
  insert(inputs, results[1]) -- FIXME: argparse is weird and does not store the results of :args("1+") arguments
  return results[1]
end

local parser = argparse()
  :description("Sift through Lua tables using a selector expression")
parser:argument "selector"
  :description("Selector expression")
  :args(1)
parser:argument "input"
  :description("Input files")
  :args("1+")
  :default("STDIN")
  :convert(safe_dofile)

local args = parser:parse()

-- Parse the selector and create a filter function
local filter = assert(lq:match(args.selector), "Parsing selector failed to produce a filter")

local outputs = {}
for _,input in ipairs(inputs) do
  local filter_outputs = assert(filter(input))
  move(filter_outputs, 1, #filter_outputs, #outputs + 1, outputs)
end

for _,output in ipairs(outputs) do
  prettyprint(output)
end
