---
--- Generated by EmmyLua(https://github.com/EmmyLua)
--- Created by liaozhaoyan.
--- Outstanding Contributor: achengli
--- DateTime: 2022/12/8 10:37 AM
---

local pystring = {}

local function checkLuaReReserve(ch)
    local luaReReserve = "().%+-*?[]^$"
    for c in string.gmatch(luaReReserve, ".") do
        if c == ch then
            return "%" .. ch
        end
    end
    return ch
end

local function setupDelimiter(delimiter)
    local rt = {}
    local i = 0
    for c in string.gmatch(delimiter, ".") do
        i = i + 1
        if c == " " then
            rt[i] = "%s"
        else
            rt[i] = checkLuaReReserve(c)
        end
    end
    return table.concat(rt)
end

local function setupPatten(s)
    local patten
    if s == nil then
        patten = "[%s\t\n]"
    else
        patten = setupDelimiter(s)
    end
    return patten
end

local function _setupRepl(repl)
    local rt = {}
    local i = 0
    for c in string.gmatch(repl, ".") do
        i = i + 1
        rt[i] = checkLuaReReserve(c)
    end
    return table.concat(rt)
end

local function setupRepl(s)
    if s == nil then
        error("repl must be a string.")
    else
        return _setupRepl(s)
    end
end

--- Rotates the characters of 's' 'n' positions circulary.
--- --
--- @param s string
--- @param n integer
--- @return string
function pystring.shift(s, n)  -- positive for right, negative for left
    local len = #s
    if len == 0 then
        return s
    end
    n = n % len
    if n == 0 then
        return s
    elseif n > 0 then  -- "abcd >> 1"
        local offset = len - n
        local s1 = string.sub(s, offset + 1)
        local s2 = string.sub(s, 1, offset)
        return s1 .. s2
    else   -- "abcd << 1"
        local offset = len + n
        local s1 = string.sub(s, offset + 1)
        local s2 = string.sub(s, 1, offset)
        return s2 .. s1
    end
end

--- True if the string has only lowercase characters, false if not.
--- --
--- @param s string
--- @return boolean
function pystring.islower(s)
    local match = string.match(s, "[%l%s%p]+")
    if not match then
        return false
    end
    return #match == #s
end

--- True if the string has only uppercase characters, false if not.
--- --
--- @param s string
--- @return boolean
function pystring.isupper(s)
    local match = string.match(s, "[%u%s%p]+")
    if not match then
        return false
    end
    return #match == #s
end

--- If the string has only digits it returns true, otherwise it will be 
--- false.
--- --
--- @param s string
--- @return boolean
function pystring.isdigit(s)
    local match = string.match(s, "%d+")
    if not match then
        return false
    end
    return #match == #s
end

--- If the string is an integer expression it returns true, otherwise false.
--- --
--- @param s string
--- @return boolean
function pystring.isinteger(s)
    local match = string.match(s, "^[%-%+]?%d+$")
    return match and true or false
end

--- If the string is an hexadecimal expression, the function returns true, 
--- otherwise it returns false.
--- --
--- @param s string
--- @return boolean
function pystring.ishex(s)
    local match = string.match(s, "%x+")
    if not match then
        return false
    end
    return #match == #s
end

--- If the string is a combination of alfanumeric characters, the function 
--- returns true, otherwise it returns false.
--- --
--- @param s string
--- @return boolean
function pystring.isalnum(s)
    local match = string.match(s, "%w+")
    if not match then
        return false
    end
    return #match == #s
end

--- If the string is a title expression, the function  returns true, otherwise
--- it returns false.
--- --
--- @param s string
--- @return boolean
function pystring.istitle(s)
    local match = string.match(s, "%u%l*")
    if not match then
        return false
    end
    return #match == #s
end

--- If the string has a float expression, the function will return true, in other
--- case it will be false.
--- --
--- @param s string
--- @return boolean
function pystring.isfloat(s)
    local re = "^[%-%+]?%d*%.%d+$"
    return string.match(s, re) ~= nil
end

--- Lua string.lower wrapper
--- --
--- @param s string
--- @return string
function pystring.lower(s)
    return string.lower(s)
end

--- Lua string.lower wrapper
--- --
--- @param s string
--- @return string
function pystring.casefold(s)
    return string.lower(s)
end

--- Lua string.upper wrapper
--- --
--- @param s string
--- @return string
function pystring.upper(s)
    return string.upper(s)
end

--- Set to upper case the lower case letters and vice versa.
--- --
--- @param s string
--- @return string
function pystring.swapcase(s)
    local swaps = {}
    local ascA, ascZ, asc_a, asc_z = string.byte('A'), string.byte('Z'), string.byte('a'), string.byte('z')
    for i=1, #s do
        local ch = string.byte(s, i)
        if ch >= ascA and ch <= ascZ  then
            swaps[i] = string.char(ch + 32)
        elseif ch >= asc_a and ch <= asc_z then
            swaps[i] = string.char(ch - 32)
        else
            swaps[i] = string.char(ch)
        end
    end
    return table.concat(swaps)
end

--- Capitalize `s` word.
--- --
--- @param s string
--- @return string
function pystring.capitalize(s)
    if #s < 1 then
        return s
    end
    local s1 = string.sub(s, 1, 1)
    local s2 = string.sub(s, 2)
    return string.upper(s1) .. s2
end

--- Capitalize all of words in `s`.
--- --
--- @param s string
--- @return string
function pystring.title(s)
    if #s < 1 then
        return s
    end
    local ss = pystring.split(s, " ")
    for i = 1, #ss do
        ss[i] = pystring.capitalize(ss[i])
    end
    return table.concat(ss, " ")
end

--- Capitalize line by line all the lines in `s`.
--- --
--- @param s string
--- @return string
function pystring.capwords(s)
    local lines = pystring.split(s, "\n")
    local rLines = {}
    for i, line in ipairs(lines) do
        local rWords = {}
        local words = pystring.split(line, " ")
        for j, word in ipairs(words) do
            rWords[j] = pystring.capitalize(word)
        end
        rLines[i] = table.concat(rWords, " ")
    end
    return table.concat(rLines, "\n")
end

--- Justify `s` by left with `len` copies of `ch`.
--- --
--- @param s string
--- @param len integer
--- @param ch string
--- @return string
function pystring.ljust(s, len, ch)
    ch = ch or " "
    if #ch ~= 1 then
        error("pad string master a single word, not " .. ch)
    end
    local delta = len - #s
    if delta > 0 then
        local pad = string.rep(ch, delta)
        return pad .. s
    else
        return s
    end
end

--- Justify `s` by right with `len` copies of `ch`.
--- --
--- @param s string
--- @param len integer
--- @param ch string
--- @return string
function pystring.rjust(s, len, ch)
    ch = ch or " "
    if #ch ~= 1 then
        error("pad string master a single word, not " .. ch)
    end
    local delta = len - #s
    if delta > 0  then
        local pad = string.rep(ch, delta)
        return s .. pad
    else
        return s
    end
end

--- Center `s` lines with `len` copies of `ch` in the longest line.
--- --
--- @param s string
--- @param len integer
--- @param ch string
--- @return string
function pystring.center(s, len, ch)
    ch = ch or " "
    if #ch ~= 1 then
        error("pad string master a single word, not " .. ch)
    end
    local delta = len - #s
    if delta > 0 then
        local left = math.floor(delta / 2)
        local right = delta - left

        local res = {string.rep(ch, left), s, string.rep(ch, right)}
        return table.concat(res)
    else
        return s
    end
end

--- Justify by left with zeros.
--- --
--- @param s string
--- @param len integer
--- @return string
function pystring.zfill(s, len)
    return pystring.ljust(s, len, "0")
end

--- Convert the given `s` string in a table of substrings 
--- delimited by `delimiter`. The maximum number of substrings is
--- defined by `n` which is 2^63 - 1 (*MaxInteger*) by default.
--- --
--- @param s string
--- @param delimiter string
--- @param n integer
function pystring.split(s, delimiter, n)
    local result = {}
    if not delimiter or delimiter == "" then  -- for blank, gsub multi blank to single
        s = string.gsub(s, "%s+", " ")
    end
    delimiter = setupDelimiter(delimiter or " ")
    n = n or 2 ^ 63 - 1

    local nums = 0
    local beg = 1
    local c = 0
    while (true) do
        local iBeg, iEnd = string.find(s, delimiter, beg)
        if (iBeg) then
            c = c + 1
            result[c] = string.sub(s, beg, iBeg - 1)
            beg = iEnd + 1
            nums = nums + 1
            if nums >= n then
                c = c + 1
                result[c] = string.sub(s, beg, #s)
                break
            end
        else
            c = c + 1
            result[c] = string.sub(s, beg, #s)
            break
        end
    end
    return result
end

--- Divide s by `del` delimiter returning the left side,
--- the delimiter and the right side.
--- --
--- @param s string
--- @param del string
--- @return table<string> | nil
function pystring.partition(s, del)
    local result = {}
    del = del or " "
    local delimiter = setupDelimiter(del)
    local iBeg, iEnd = string.find(s, delimiter)
    if iBeg then
        result[1] = string.sub(s, 1, iBeg - 1)
        result[2] = del
        result[3] = string.sub(s, iEnd + 1)
        return result
    else
        return nil
    end
end

local function reverseTable(t)
    local n = #t
    for i = 1, n / 2 do
        t[i], t[n + 1 - i] = t[n + 1 - i], t[i]
    end
end

--- Split the reverse of `s` with the reverse of `delimiter`
--- --
--- @param s string
--- @param delimiter string
--- @param n integer
--- @return table<string>
function pystring.rsplit(s, delimiter, n)
    local result = {}
    local n = n or 2 ^ 63 - 1
    local len = #s + 1
    local rs = string.reverse(s)
    local rDel = string.reverse(delimiter or " ")
    rDel = setupDelimiter(rDel)
    local nums = 0
    local beg = 1
    local c = 0

    while (true) do
        local iBeg, iEnd = string.find(rs, rDel, beg)
        if (iBeg) then
            c = c + 1
            result[c] = string.sub(s, len - (iBeg - 1),len - beg)
            beg = iEnd + 1
            nums = nums + 1
            if nums >= n then
                c = c + 1
                result[c] = string.sub(s, 1, len - beg)
                break
            end
        else
            c = c + 1
            result[c] = string.sub(s, 1, len - beg)
            break
        end
    end
    --return result
    reverseTable(result)
    return result
end

--- 
function pystring.rpartition(s, del)
    local result = {}
    del = del or " "
    local rs = string.reverse(s)
    local rDel = string.reverse(del)
    local delimiter = setupDelimiter(rDel)
    local len = #s

    local iBeg, iEnd = string.find(rs, delimiter)
    if iBeg then
        result[1] = string.sub(s, 1, len - iBeg + 1 - #del)
        result[2] = del
        result[3] = string.sub(s, len - iEnd + 1 + #del)
        return result
    else
        return nil
    end
end

--- Split string line by line
--- --
--- @param s string
--- @return table<string>
function pystring.splitlines(s)
    return pystring.split(s, '\n')
end

--- Remove first `chars` string of `s`. 
--- --
--- @param s string
--- @param chars string
--- @return string
function pystring.lstrip(s, chars)
    local patten = "^" .. setupPatten(chars) .. "+"
    local _, ends = string.find(s, patten)
    if ends then
        return string.sub(s, ends + 1, -1)
    else
        return s
    end
end

--- Remove last `chars` string of `s`.
--- --
--- @param s string
--- @param chars string
--- @return string
function pystring.rstrip(s, chars)
    local patten = setupPatten(chars) .. "+$"
    local last = string.find(s, patten)
    if last then
        return string.sub(s, 1, last - 1)
    else
        return s
    end
end

--- Remove last and first `chars` string of `s`, it's a consecutive
--- `lstrip` and `rstrip`.
--- --
--- @param s string
--- @param chars string
--- @return string
function pystring.strip(s, chars)
    local res = pystring.lstrip(s, chars)
    return pystring.rstrip(res, chars)
end

--- Joins an array of *string* `strings` with `delim` between.
--- --
--- @param delim string
--- @param strings table<string>
--- @return string
function pystring.join(delim, strings)
    return table.concat(strings, delim)
end

--- Check if `s1` begin with `s2`.
--- --
--- @param s1 string
--- @param s2 string
--- @return string | boolean
function pystring.startswith(s1, s2)
    return string.sub(s1,1, #s2) == s2
end

--- Check if `s1` ends with `s2`.
--- --
--- @param s1 string
--- @param s2 string
--- @return string | boolean
function pystring.endswith(s1, s2)
    return s2 == '' or string.sub(s1,-#s2) == s2
end

--- Get the first ocurrence of `s2` in `s1` beginning from `start`
--- and finishing at `stop`.
--- --
--- @param s1 string
--- @param s2 string
--- @param start integer
--- @param stop integer
--- @return integer
function pystring.find(s1, s2, start, stop)
    start = start or 1
    stop = stop or -1
    s1 = string.sub(s1, start, stop)
    local res = string.find(s1, s2, 1, false)
    return res or -1
end

--- Get the first ocurrence of `s2` in `s1` beginning from `start`
--- and finishing at `stop` but working with the reversed version of
--- `s1`.
--- --
--- @param s1 string
--- @param s2 string
--- @param start integer
--- @param stop integer
--- @return integer
function pystring.rfind(s1, s2, start, stop)
    start = start or 1
    stop = stop or -1
    s1 = string.sub(s1, start, stop)

    local len = #s1
    local lFind = #s2
    local rs1, rs2 = string.reverse(s1), string.reverse(s2)
    local i = string.find(rs1, rs2, 1, false)
    if i then
        return len - i - lFind + 1
    else
        return -1
    end
end

--- Get the first ocurrence of `s2` in `s1` starting at `start`
--- and finishing at `stop`.
--- --
--- @param s1 string
--- @param s2 string
--- @param start integer
--- @param stop integer
--- @return integer
function pystring.index(s1, s2, start, stop)
    local res = pystring.find(s1, s2, start, stop)
    if res < 0 then
        error(s2 .. " is  not in " .. s1)
    end
    return res
end

--- Get the index of first `s2` ocurrence in `s1` beginning from `start`
--- and ending at `stop`
--- --
--- @param s1 string
--- @param s2 string
--- @param start integer
--- @param stop integer
--- @return integer
function pystring.rindex(s1, s2, start, stop)
    local res = pystring.rfind(s1, s2, start, stop)
    if res < 0 then
        error(s2 .. " is  not in " .. s1)
    end
    return res
end

--- Count how many times the pattern appears in the target
--- string.
--- --
--- @param s string
--- @param find string
--- @return integer
function pystring.count(s, find)
    local i = 0
    local patten = setupPatten(find)
    for _ in string.gmatch(s, patten) do
        i = i + 1
    end
    return i
end

--- Replaces the first n occurrences which matches with 'find' pattern
--- and substitutes them by repl.
--- --
--- @param s string
--- @param find string # Regular expression
--- @param repl string # Replacement
--- @param n integer # Number of occurrences until stop counting.
--- @return string, integer
function pystring.replace(s, find, repl, n)
    local patten = setupPatten(find)
    repl = setupRepl(repl)

    return string.gsub(s, patten, repl, n)
end

--- Expand blank spaces in the string by 'tabs' times.
--- --
--- @param s string
--- @param tabs integer
--- @return string, integer
function pystring.expandtabs(s, tabs)
    tabs = tabs or 4
    local repl = string.rep(" ", tabs)
    return string.gsub(s, "\t", repl)
end

--- Focus on the file content without worrying about the file descriptor.
--- `executor` function evals every line if `mode` is set to "line" or the full
--- file content if it is set to "raw" or `nil`.
--- --
--- @param file_name string # Name of the file
--- @param executor function # Function that works with the file descriptor
--- @param mode string # {"lines", "raw" | nil} How the file will be processed.
--- @param file_opt string # File options. See `io.open`
--- @return any
function pystring.with(file_name, executor, mode, file_opt)
    local f = io.open(file_name, file_opt or 'r')
    local r = nil
    if file_opt and file_opt:match('w') then
        error"pystring.with doesn't work with writing mode files"
    end
    if f then
        if mode == 'lines' then
            for l in f:lines("l") do
                r = executor(l,r) -- note that r can be skipped on executor implementation
            end
        elseif (not mode or mode == 'raw') then
            local _raw_file = f:read("*a")
            r = executor(_raw_file)
        else
            error(string.format('Invalid mode = %s option',mode))
        end
    else
        error("Problems opening " .. file_name)
    end
    f:close()
    return r
end

return pystring
