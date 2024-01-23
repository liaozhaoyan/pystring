---
--- Generated by EmmyLua(https://github.com/EmmyLua)
--- Created by liaozhaoyan.
--- DateTime: 2023/1/3 9:31 PM
---

package.path = package.path .. ";../src/?.lua;"

local pystring = require("pystring")

-- split function
local ret = pystring.split("hello lua language")
assert(#ret == 3)
assert(ret[1] == "hello")
assert(ret[2] == "lua")
assert(ret[3] == "language")
ret = pystring.split("hello lua language lua language")
assert(#ret == 5 )
assert(ret[1] == "hello")
assert(ret[2] == "lua")
assert(ret[3] == "language")
assert(ret[4] == "lua")
assert(ret[5] == "language")
ret = pystring.split("Node 0, zone      DMA      1      0      0      1      2      1      1      0      1      1      3")
assert(#ret == 15)

-- spit by delimiter arg
ret = pystring.split("hello*lua *language", "*")
assert(#ret == 3)
assert(ret[1] == "hello")
assert(ret[2] == "lua ")
assert(ret[3] == "language")

-- rsplit by arg number
ret = pystring.rsplit("hello*lua *language", "*", 1)
assert(#ret == 2)
assert(ret[1] == "hello*lua ")
ret = pystring.rsplit("hello*lua *lua language", "lua", 1)
assert(#ret == 2)
assert(ret[1] == "hello*lua *")

-- split by multi string
ret = pystring.split("hello*lua *language", "*l")
assert(#ret == 3)
assert(ret[1] == "hello")
assert(ret[2] == "ua ")
assert(ret[3] == "anguage")

-- partition
ret = pystring.partition("hello lua")
assert(#ret == 3)
assert(ret[1] == "hello")
assert(ret[2] == " ")
assert(ret[3] == "lua")
ret = pystring.partition("hello*lua", "*")
assert(#ret == 3)
assert(ret[1] == "hello")
assert(ret[2] == "*")
assert(ret[3] == "lua")
ret = pystring.partition("hello lua language")
assert(#ret == 3)
assert(ret[1] == "hello")
assert(ret[2] == " ")
assert(ret[3] == "lua language")
ret = pystring.partition("hello lua language", "lua")
assert(#ret == 3)
assert(ret[1] == "hello ")
assert(ret[2] == "lua")
assert(ret[3] == " language")
ret = pystring.partition("hello*lua")
assert(ret == nil)

-- rpartition
ret = pystring.rpartition("hello lua language")
assert(ret[1] == "hello lua")
assert(ret[2] == " ")
assert(ret[3] == "language")
ret = pystring.rpartition("hello lua lua language", "lua")
assert(ret[1] == "hello lua ")
assert(ret[2] == "lua")
assert(ret[3] == " language")

-- splitlines
ret = pystring.splitlines("hello\nlua\nlanguage")
assert(ret[1] == "hello")
assert(ret[2] == "lua")
assert(ret[3] == "language")

-- strip with default args.
assert(pystring.strip("\t hello world.  \t\n") == "hello world.")

-- strip with args
assert(pystring.strip("**hello world**", "*") == "hello world")

-- strip with multi type string
assert(pystring.strip("*?hello world*?", "*?") == "hello world")

-- strip string
assert(pystring.strip("abcdefhello worldabcdef", "abcdef") == "hello world")

-- lstrip string
assert(pystring.lstrip("abcdefhello worldabcdef", "abcdef") == "hello worldabcdef")

-- rstrip string
assert(pystring.rstrip("abcdefhello worldabcdef", "abcdef") == "abcdefhello world")

-- join string
local s = "abc d ef g"
local ret = pystring.split(s)
assert(pystring.join(" ", ret) == s)

-- startswith/endswith
assert(pystring.startswith("hello world", "hello"))
assert(pystring.endswith("hello world", "world"))

-- find
assert(pystring.find("hello world.", "hello") == 1)
assert(pystring.find("hello world.", "hEllo") == -1)

-- rfind
assert(pystring.rfind("hello world hello.", "hello") == 12)
assert(pystring.rfind("hello world hello.", "hEllo") == -1)

-- count
assert(pystring.count("hello world hello.", "hello") == 2)
assert(pystring.count("hello world hello.", "hEllo") == 0)
assert(pystring.count("hello world hello.", " ") == 2)

-- shift
assert(pystring.shift("abcd", 1) == "dabc")
assert(pystring.shift("abcd", -1) == "bcda")
assert(pystring.shift("abcd", -2) == "cdab")

-- swapcase
assert(pystring.swapcase("Hello, World!") == "hELLO, wORLD!")

-- capitalize
assert(pystring.capitalize("hello") == "Hello")
assert(pystring.capitalize("") == "")
assert(pystring.capitalize("H") == "H")

-- title
assert(pystring.title("hello") == "Hello")
assert(pystring.title("") == "")
assert(pystring.title("hello world.") == "Hello World.")
assert(pystring.title("hello  world.") == "Hello  World.")

-- capwords
assert(pystring.capwords("hello world.") == "Hello World.")
assert(pystring.capwords("hello world.\nhere  you are.") == "Hello World.\nHere  You Are.")

-- islower
assert(pystring.islower("hello") == true)
assert(pystring.islower("Hello") == false)
assert(pystring.islower("hello world!") == true)

-- isupper
assert(pystring.isupper("HELLO") == true)
assert(pystring.isupper("Hello") == false)
assert(pystring.isupper("Hello World") == false)
assert(pystring.isupper("HELLO WORLD!") == true)

-- isdigit
assert(pystring.isdigit("1234") == true)
assert(pystring.isdigit("123a") == false)
assert(pystring.isdigit("123.45") == false)

-- ishex
assert(pystring.ishex("1234") == true)
assert(pystring.ishex("123a") == true)
assert(pystring.ishex("abcdef") == true)
assert(pystring.ishex("00ABCDEF") == true)
assert(pystring.ishex("123FG") == false)
assert(pystring.ishex("123.45") == false)

-- isalnum
assert(pystring.isalnum("1234") == true)
assert(pystring.isalnum("00ABCDEF") == true)
assert(pystring.isalnum("123FG") == true)
assert(pystring.isalnum("123.45") == false)
assert(pystring.isalnum("123 45") == false)

-- istitle
assert(pystring.istitle("Aaa") == true)
assert(pystring.istitle("aaa") == false)
assert(pystring.istitle("Aaa0") == false)
assert(pystring.istitle("A") == true)

-- isfloat
assert(pystring.isfloat("1234") == false)
assert(pystring.isfloat("00ABCDEF") == false)
assert(pystring.isfloat("123FG") == false)
assert(pystring.isfloat("123.45") == true)
assert(pystring.isfloat("123 45") == false)

-- ljust
assert(pystring.ljust("1234", 5) == " 1234")
assert(pystring.ljust("1234", 3) == "1234")
assert(pystring.ljust("1234", 6, "*") == "**1234")

-- rjust
assert(pystring.rjust("1234", 5) == "1234 ")
assert(pystring.rjust("1234", 3) == "1234")
assert(pystring.rjust("1234", 6, "*") == "1234**")

-- center
assert(pystring.center("1234", 5) == "1234 ")
assert(pystring.center("1234", 7) == " 1234  ")
assert(pystring.center("1234", 8) == "  1234  ")
assert(pystring.center("1234", 8, "*") == "**1234**")

-- zfill
assert(pystring.zfill("3.14", 6) == "003.14")

-- replace
assert(pystring.replace("hello world.", "world", "lua") == "hello lua.")
assert(pystring.replace("hello world world.", "world", "lua") == "hello lua lua.")
assert(pystring.replace("hello world world.", "world", "lua", 1) == "hello lua world.")
assert(pystring.replace("hello %. %*.", "%.", "%*") == "hello %* %*.")
assert(pystring.replace("hello %. %*.", "%.", " ") == "hello   %*.")

-- expandtabs
assert(pystring.expandtabs("hello\tworld.") == "hello    world.")

-- with
local file = 'test_file.txt'
local content = "hello there!\nWhat's up?"
local f = io.open(file,'w')
if not f then
    error(string.format("io.open failed opening %s file in write mode",file))
end
f:write(content)
f:close()
local withContent = pystring.with(file)
assert(withContent == content)
withContent = pystring.with(file, nil, function(s) return pystring.lower(s) end)
assert(withContent == pystring.lower(content))
local withTable = pystring.with(file, "lines")
assert(pystring.join("\n", withTable) == content)
withTable = pystring.with(file, "lines", function(s) return pystring.upper(s) end)
assert(pystring.join("\n", withTable) == pystring.upper(content))
os.execute('rm ' .. file)

print("test ok.")
