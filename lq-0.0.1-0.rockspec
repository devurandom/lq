package = "lq"
version = "0.0.1-0"
source = {
  url = "https://github.com/urzds/lq/archive/v0.0.1-0.tar.gz",
  dir = "lq-0.0.1-0"
}
description = {
  summary = "command-line Lua table processor",
  detailed = [[
    lq is for Lua what jq is for JSON
  ]],
  homepage = "https://github.com/urzds/lq/",
  license = "MIT <http://opensource.org/licenses/MIT>"
}
dependencies = {
  "lpeg >= 1.0"
}
build = {
  type = "none",
  install = {
    bin = {
      lq = "lq.lua",
    }
  },
}
