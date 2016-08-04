# lq

command-line Lua table processor (similar to [jq](https://stedolan.github.io/jq/))

## Usage example

```lua
return {
  field = {
    number = 1,
    string = "str",
  },
}
```

```sh
# ./lq.lua .field.number input.lua
1
# ./lq.lua '.field|keys' input.lua
number
string
# ./lq.lua '.field.[]' input.lua
1
str
```
