# lq

command-line Lua table processor (similar to [jq](https://stedolan.github.io/jq/))

## Usage example

```lua
return {
  field = {
    number = 1,
    string = "str",
  },
  advanced = {
    { a = 1, b = 2, },
    { a = 3, b = 4, c = 11, },
    { a = 5, b = 6, },
  },
}
```

```sh
# lq .field.number input.lua
1
# lq '.field | keys' input.lua
number
string
# lq '.field | values' input.lua
1
str
# lq '.field.[]' input.lua
1
str
# lq '.advanced.[] | filter(.a == 1) | .b?'
2
# lq '.advanced.[] | (if has("c") then [.b] else [1,2] end)'
{
  1,
  2
}
{
  4
}
{
  1,
  2
}
```

## Install

Run `luarocks install lq` or check-out this repository and run `luarocks make`.
