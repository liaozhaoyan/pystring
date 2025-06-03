#define LUA_LIB
#include <lua.h>
#include <lauxlib.h>
#include <stdio.h>
#include <string.h>
#include <stddef.h>
#include <ctype.h>
#include "utarray.h"

struct ut_nstr {
    const char *s;
    size_t len;
};

static void utarray_nstr_cpy(void *dst, const void *src) {
    struct ut_nstr *srcc = (struct ut_nstr *)src;
    char **dstc = (char**)dst;
    if (srcc == NULL) {
        *dstc = NULL;
    } else {
        *dstc = (char*)malloc(srcc->len + 1);
        if (*dstc == NULL) {
            utarray_oom();
        } else {
            strncpy(*dstc, srcc->s, srcc->len);
        }
        (*dstc)[srcc->len] = '\0';
    }
}

static const UT_icd ut_nstr_icd UTARRAY_UNUSED = {sizeof(char*),NULL,utarray_nstr_cpy,utarray_str_dtor};

#if LUA_VERSION_NUM < 502
# define luaL_newlib(L,l) (lua_newtable(L), luaL_register(L,NULL,l))
# define lua_rawlen lua_objlen
#endif

/* fastsearch implementation copied from python's stringlib. */

/* fast search/count implementation, based on a mix between boyer-
   moore and horspool, with a few more bells and whistles on the top.
   for some more background, see: http://effbot.org/zone/stringlib.htm */

/* note: fastsearch may access string[n], which isn't a problem when using
   strings without a trailing \0.
   the count mode returns -1
   if there cannot possible be a match in the target string, and 0 if
   it has actually checked for matches, but didn't find any. */

#define FAST_COUNT 0
#define FAST_SEARCH 1
#define FAST_RSEARCH 2

#define BLOOM_ADD(mask, ch) ((mask |= (1 << ((ch) & 0x1F))))
#define BLOOM(mask, ch)     ((mask &  (1 << ((ch) & 0x1F))))

static ssize_t fastsearch(const char* string, size_t slen,
           const char* token, size_t tlen,
           size_t maxcount, int mode) {
    // set maxcount = -1 to find all.
    long mask;
    size_t skip, count = 0;
    ssize_t i, j, tlast; 

    ssize_t w = slen - tlen;

    if ((w < 0) || (mode == FAST_COUNT && maxcount == 0)) {
        return -1;
    }

    /* look for special cases */
    if (tlen <= 1) {
        // empty search string.
        if (tlen <= 0) return -1;

        char ch = token[0];
        switch (mode)
        {
        case FAST_COUNT:
            for (i = 0; i < slen; i++) {
                if (string[i] == ch) {
                    count++;
                    if (count == maxcount) return maxcount;
                }
            }
            return count;
        case FAST_SEARCH:
            for (i = 0; i < slen; i++) {
                if (string[i] == ch) return i;
            }
            return -1;

        case FAST_RSEARCH:
            for (i = slen - 1; i > -1; i--) {
                if (string[i] == ch) return i;
            }
            return -1;
        
        default:
            return -1;
        }
    }

    tlast = tlen - 1;
    skip = tlast - 1;
    mask = 0;

    if (mode != FAST_RSEARCH) { // FAST_SEARCH | COUNT

        /* create compressed boyer-moore delta 1 table */

        /* process pattern[:-1] */
        for (i = 0; i < tlast; i++) {
            BLOOM_ADD(mask, token[i]);
            if (token[i] == token[tlast]) skip = tlast - i - 1;
        }
        /* process pattern[-1] outside the loop */
        BLOOM_ADD(mask, token[tlast]);

        for (i = 0; i <= w; i++) {
            /* note: using tlast in the skip path slows things down on x86 */
            if (string[i+tlen-1] == token[tlen-1]) {
                /* candidate match */
                for (j = 0; j < tlast; j++) {
                    if (string[i+j] != token[j]) break;
                }
                if (j == tlast) {
                    /* got a match! */
                    if (mode != FAST_COUNT) return i;
                    count++;
                    if (count == maxcount) return maxcount;
                    i = i + tlast;
                    continue;
                }
                /* miss: check if next character is part of pattern */
                if (!BLOOM(mask, string[i+tlen])) i = i + tlen;
                else i = i + skip;
            } else {
                /* skip: check if next character is part of pattern */
                if (!BLOOM(mask, string[i+tlen])) i = i + tlen;
            }
        }
    } else {    /* FAST_RSEARCH */

        /* create compressed boyer-moore delta 1 table */

        /* process pattern[0] outside the loop */
        BLOOM_ADD(mask, token[0]);
        /* process pattern[:0:-1] */
        for (i = tlast; i > 0; i--) {
            BLOOM_ADD(mask, token[i]);
            if (token[i] == token[0]) skip = i - 1;
        }

        for (i = w; i >= 0; i--) {
            if (string[i] == token[0]) {
                /* candidate match */
                for (j = tlast; j > 0; j--) {
                    if (string[i+j] != token[j]) break;
                }
                /* got a match! */
                if (j == 0) return i;
                /* miss: check if previous character is part of pattern */
                if (!BLOOM(mask, string[i-1])) i = i - tlen;
                else i = i - skip;
            } else {
                /* skip: check if previous character is part of pattern */
                if (!BLOOM(mask, string[i-1])) i = i - tlen;
            }
        }
    }

    if (mode != FAST_COUNT)
        return -1;
    return count;
}

static ssize_t check_int(lua_State *L, int arg, ssize_t default_v) {
    int type = lua_type(L, arg);
    if (type == LUA_TNUMBER) {
        return lua_tonumber(L, arg);
    } else if (type == LUA_TNIL) {
        return default_v;
    }
    else {
        luaL_argerror(L, arg, "number expected");
        return 0;
    }
}

static int get_position(lua_State *L, 
    size_t string_len, 
    size_t tocken_len, 
    const int start_arg,
    const int end_arg,
    ssize_t * pstart, 
    ssize_t * pend) {
    int istring_len = string_len;

    size_t nargs = lua_gettop(L);
    ssize_t start = (nargs > start_arg - 1) ? check_int(L, start_arg, 1): 1;  //c index start from 0
    if (start > 0) {
        if (start > string_len) {  // if start is greater than string length, return 0
            return -1;
        }
        start --;  // should be 0-based
    } else if (start < 0) { 
        start = start < -istring_len ? -istring_len : start;  // fix negative index, never less than 0
        start = string_len + start; 
    }

    ssize_t end = (nargs > end_arg - 1) ? check_int(L, end_arg, string_len): string_len;  //c index start from 0, end offset should be greater than start
    if (end > istring_len) {
        end = istring_len;
    }
    if (end < 0) { 
        if (end < -string_len) {  // if end is less than -string length, return 0
            return -1;
        }
        end = string_len + end + 1;
    }
    if (start >= end) {
        return -1;
    }
    *pstart = start;
    *pend = end;
    return 0;
}

static const char* check_strict_string(lua_State *L, int arg, size_t * len) {
    int type = lua_type(L, arg);
    if (type == LUA_TSTRING) {
        return lua_tolstring(L, arg, len);
    } else {
        luaL_argerror(L, arg, "string expected");
        return NULL;
    }
}

// count(string, token, [start, [end]])
static int count(lua_State *L) {
    size_t string_len, token_len;
    const char *string = check_strict_string(L, 1, &string_len);
    const char *token = check_strict_string(L, 2, &token_len);
    ssize_t start, end;

    if (get_position(L, string_len, token_len, 3, 4, &start, &end) < 0) {
        goto fallReturn;
    }
    string_len = end - start;
    ssize_t cnt = fastsearch(string + start, string_len, token, token_len, -1, FAST_COUNT);
    cnt = cnt > 0 ? cnt : 0;
#if LUA_VERSION_NUM >= 503
    lua_pushinteger(L, cnt);
#else
    lua_pushnumber(L, cnt);
#endif
    return 1;

fallReturn:
#if LUA_VERSION_NUM >= 503
    lua_pushinteger(L, 0);
#else
    lua_pushnumber(L, 0);
#endif
    return 1;
}

// find(string, token, [start, [end]])
static int find(lua_State *L) {
    size_t string_len, token_len;
    const char *string = check_strict_string(L, 1, &string_len);
    const char *token = check_strict_string(L, 2, &token_len);
    ssize_t start, end;

    if (get_position(L, string_len, token_len, 3, 4, &start, &end) < 0) {
        goto fallReturn;
    }

    ssize_t pos = fastsearch(string + start, end - start, token, token_len, 1, FAST_SEARCH);
    if (pos >= 0) {   // find  renturn 1-based, not find return -1;
        pos += 1;
    }
#if LUA_VERSION_NUM >= 503
    lua_pushinteger(L, pos);
#else
    lua_pushnumber(L, pos);
#endif
    return 1;

fallReturn:
#if LUA_VERSION_NUM >= 503
    lua_pushinteger(L, -1);
#else
    lua_pushnumber(L, -1);
#endif
    return 1;
}

//rfind (string, token, [start, [end]])
static int rfind(lua_State *L) {
    size_t string_len, token_len;
    const char *string = check_strict_string(L, 1, &string_len);
    const char *token = check_strict_string(L, 2, &token_len);
    ssize_t start, end;
    if (get_position(L, string_len, token_len, 3, 4, &start, &end) < 0) {
        goto fallReturn;
    }
    ssize_t pos = fastsearch(string + start, end - start, token, token_len, 1, FAST_RSEARCH);
    pos = pos >= 0 ? pos + 1 : -1;
#if LUA_VERSION_NUM >= 503
    lua_pushinteger(L, pos);
#else
    lua_pushnumber(L, pos);
#endif
    return 1;

fallReturn:
    #if LUA_VERSION_NUM >= 503
        lua_pushinteger(L, -1);
    #else
        lua_pushnumber(L, -1);
    #endif
        return 1;
}

static int replace(lua_State *L) {
    size_t string_len, old_len, new_len;
    const char *string = check_strict_string(L, 1, &string_len);
    const char *old = check_strict_string(L, 2, &old_len);
    if (old_len == 0) {
        luaL_argerror(L, 2, "empty string");
    }
    const char *new = check_strict_string(L, 3, &new_len);

    ssize_t count, max_replace, offset, last;
    size_t nargs = lua_gettop(L);
    if (nargs >= 4) {
        max_replace = check_int(L, 4, string_len);
        if (max_replace <= 0) {
            lua_pushstring(L, string);
            return 1;
        }
    } else {
        max_replace = string_len;
    }

    count = 0;
    offset = 0;
    last = 0;
    luaL_Buffer buffer;
    luaL_buffinit(L, &buffer);
    while (count < max_replace) {
        offset = fastsearch(string + last, string_len - last, old, old_len, 1, FAST_SEARCH);
        if (offset < 0) {
            break;
        } else {
            luaL_addlstring(&buffer, string + last, offset);
            if (new_len > 0) {
                luaL_addlstring(&buffer, new, new_len);
            }
            last = last + offset + old_len;
        }
        count ++;
    }
    if (last < string_len) {
        luaL_addlstring(&buffer, string + last, string_len - last);
    }
    luaL_pushresult(&buffer);
    return 1;
}

static int rreplace(lua_State *L) {
    size_t string_len, old_len, new_len;
    const char *string = check_strict_string(L, 1, &string_len);
    const char *old = check_strict_string(L, 2, &old_len);
    if (old_len == 0) {
        luaL_argerror(L, 2, "empty string");
    }
    const char *new = check_strict_string(L, 3, &new_len);

    ssize_t count, max_replace, offset, last;
    size_t nargs = lua_gettop(L);
    if (nargs >= 4) {
        max_replace = check_int(L, 4, string_len);
        if (max_replace <= 0) {
            lua_pushstring(L, string);
            return 1;
        }
    } else {
        max_replace = string_len;
    }

    count = 0;
    offset = 0;
    last = string_len;

    struct ut_nstr node;
    UT_array *strs;
    utarray_new(strs, &ut_nstr_icd);
    while (count < max_replace) {
        offset = fastsearch(string, last, old, old_len, 1, FAST_RSEARCH);
        if (offset < 0) {
            break;
        } else {
            node.s = string + offset + old_len;
            node.len = last - offset - old_len;
            utarray_insert(strs, &node, 0);
            if (new_len > 0) {
                node.s = new;
                node.len = new_len;
                utarray_insert(strs, &node, 0);
            }
            last = offset;
        }
        count ++;
    }
    if (last > 0) {
        node.s = string;
        node.len = last;
        utarray_insert(strs, &node, 0);
    }

    luaL_Buffer buffer;
    luaL_buffinit(L, &buffer);
    char **p;
    for (p = (char**)utarray_front(strs); p != NULL; p = (char**)utarray_next(strs, p)) {
        luaL_addstring(&buffer, *p);
    }
    utarray_free(strs);
    luaL_pushresult(&buffer);
    return 1;
}

static int startswith(lua_State *L) {
    size_t string_len, token_len;
    const char *string = check_strict_string(L, 1, &string_len);
    const char *token = check_strict_string(L, 2, &token_len);

    ssize_t start, end;
    if (get_position(L, string_len, token_len, 3, 4, &start, &end) < 0) {
        lua_pushboolean(L, 0);
        return 1;
    }

    ssize_t sstring_len = end - start;
    if (sstring_len >= (ssize_t)token_len && strncmp(string + start, token, token_len) == 0) {
        lua_pushboolean(L, 1);
    } else {
        lua_pushboolean(L, 0);
    }
    return 1;
}

static int endswith(lua_State *L) {
    size_t string_len, token_len;
    const char *string = check_strict_string(L, 1, &string_len);
    const char *token = check_strict_string(L, 2, &token_len);

    ssize_t start, end;
    if (get_position(L, string_len, token_len, 3, 4, &start, &end) < 0) {
        lua_pushboolean(L, 0);
        return 1;
    }

    size_t sstring_len = end - start;
    if (sstring_len >= (ssize_t)token_len && strncmp(string + start + sstring_len - token_len, token, token_len) == 0) {
        lua_pushboolean(L, 1);
    } else {
        lua_pushboolean(L, 0);
    }
    return 1;
}

static void add_buffer_chars(luaL_Buffer *B, char ch, ssize_t n) {
    switch (n) {
    case 0:
        return;
    
    case 1:
        luaL_addchar(B, ch);
        return;
    
    default:
        while (n > 0) {
            char *p = luaL_prepbuffer(B);
            size_t chunk = (n > LUAL_BUFFERSIZE) ? LUAL_BUFFERSIZE : n;
            memset(p, ch, chunk);
            luaL_addsize(B, chunk);
            n -= chunk;
        }
        break;
    }
}

static int center(lua_State *L) {
    size_t string_len;
    const char *string = check_strict_string(L, 1, &string_len);
    ssize_t width = luaL_checkinteger(L, 2);
    if (width <= 0) {
        lua_pushstring(L, string);
        return 1;
    }
    char pad_ch = ' ';
    if (lua_gettop(L) >= 3 && lua_isstring(L, 3)) {
        size_t pads_len;
        const char *pads = check_strict_string(L, 3, &pads_len);
        if (pads_len > 0) {
            pad_ch = pads[0];
        } else {
            luaL_error(L, "pads is empty string.");
        }
    }
    luaL_Buffer buffer;
    luaL_buffinit(L, &buffer);
    ssize_t pad = width - string_len;
    if (pad > 0) {
        ssize_t left = pad / 2;
        ssize_t right = pad - left;
        add_buffer_chars(&buffer, pad_ch, left);
        luaL_addstring(&buffer, string);
        add_buffer_chars(&buffer, pad_ch, right);
    } else {
        luaL_addstring(&buffer, string);
    }
    luaL_pushresult(&buffer);
    return 1;
}

static int ljust(lua_State *L) {
    size_t string_len;
    const char *string = check_strict_string(L, 1, &string_len);
    ssize_t width = luaL_checkinteger(L, 2);
    if (width <= 0) {
        lua_pushstring(L, string);
        return 1;
    }
    char pad_ch = ' ';
    if (lua_gettop(L) >= 3 && lua_isstring(L, 3)) {
        size_t pads_len;
        const char *pads = check_strict_string(L, 3, &pads_len);
        if (pads_len > 0) {
            pad_ch = pads[0];
        } else {
            luaL_error(L, "pads is empty string.");
        }
    }
    luaL_Buffer buffer;
    luaL_buffinit(L, &buffer);
    luaL_addstring(&buffer, string);
    ssize_t pad = width - string_len;
    if (pad > 0) {
        add_buffer_chars(&buffer, pad_ch, pad);
    }
    luaL_pushresult(&buffer);
    return 1;
}

static int rjust(lua_State *L) {
    size_t string_len;
    const char *string = check_strict_string(L, 1, &string_len);
    ssize_t width = luaL_checkinteger(L, 2);
    if (width <= 0) {
        lua_pushstring(L, string);
        return 1;
    }
    char pad_ch = ' ';
    if (lua_gettop(L) >= 3 && lua_isstring(L, 3)) {
        size_t pads_len;
        const char *pads = check_strict_string(L, 3, &pads_len);
        if (pads_len > 0) {
            pad_ch = pads[0];
        } else {
            luaL_error(L, "pads is empty string.");
        }
    }
    luaL_Buffer buffer;
    luaL_buffinit(L, &buffer);
    ssize_t pad = width - string_len;
    if (pad > 0) {
        add_buffer_chars(&buffer, pad_ch, pad);
    }
    luaL_addstring(&buffer, string);
    luaL_pushresult(&buffer);
    return 1;
}

static int shift(lua_State *L) {
    size_t string_len;
    const char *string = check_strict_string(L, 1, &string_len);
    ssize_t shift = luaL_checkinteger(L, 2);
    shift = shift % string_len;
    if (shift == 0) {
        lua_pushstring(L, string);
    } else if ( shift > 0) {
        luaL_Buffer buffer;
        ssize_t offset = string_len - shift;
        luaL_buffinit(L, &buffer);
        luaL_addstring(&buffer, string + offset);
        luaL_addlstring(&buffer, string, offset);
        luaL_pushresult(&buffer);
    } else {
        luaL_Buffer buffer;
        luaL_buffinit(L, &buffer);
        luaL_addstring(&buffer, string + shift);
        luaL_addlstring(&buffer, string, shift);
        luaL_pushresult(&buffer);
    }
    
    return 1;
}

static int swapcase(lua_State *L) {
    size_t string_len;
    const char *string = check_strict_string(L, 1, &string_len);
    luaL_Buffer buffer;
    luaL_buffinit(L, &buffer);
    size_t i;
    for (i = 0; i < string_len; i++) {
        char ch = string[i];
        if (isupper(ch)) {
            luaL_addchar(&buffer, tolower(ch));
        } else {
            luaL_addchar(&buffer, toupper(ch));
        }
    }
    luaL_pushresult(&buffer);
    return 1;
}

static int is_nil_split(char ch) {
    switch (ch) {
    case ' ':
    case '\t':
    case '\n':
    case '\r':
    case '\f':
    case '\v':
        return 1;
    
    default:
        return 0;
    }
}

// string_len > 0 forever
// max_split never less than 0
static int split_nil(lua_State *L, const char *string, size_t string_len, ssize_t max_split) {
    ssize_t i, last, count;
    last = 0;
    count = 0;

    if (is_nil_split(string[0])) {
        for (i = 1; i < string_len; i ++) {
            last = i;
            if (!is_nil_split(string[i])) {
                break;
            }
        }
        if (i == string_len) {
            return count;
        }
    }
    // string[i] is not nil_split
    for (i = last; i < string_len; i ++) {
        if (is_nil_split(string[i])) {
            if (i > last) {
                lua_pushlstring(L, string + last, i - last);
                lua_rawseti(L, -2, ++ count);
                last = i + 1;
                if (count == max_split) {
                    while (is_nil_split(string[last])) {
                        last ++;
                    }
                    break;
                }
            } else {
                last = i + 1;
            }
        }
    }
    if (last < string_len) {  // edge case
        lua_pushlstring(L, string + last, string_len - last);
        lua_rawseti(L, -2, ++ count);
    }
    return count;
}

static int split_ch(lua_State *L, const char *string, size_t string_len, char ch, ssize_t max_split) {
    ssize_t i, last, count;
    last = 0;
    count = 0;
    for (i = 0; i < string_len; i ++) {
        if (string[i] == ch) {
            if (last == i) {
                lua_pushstring(L, "");
            } else {
                lua_pushlstring(L, string + last, i - last);
            }
            lua_rawseti(L, -2, ++ count);
            last = i + 1;
            if (count == max_split) {
                break;
            }
        }
    }
    if (last <= string_len) {  // edge case should add last empty string
        lua_pushlstring(L, string + last, string_len - last);
        lua_rawseti(L, -2, ++ count);
    }
    return count;
}

static int split_str(lua_State *L, const char *string, size_t string_len, const char *token, size_t token_len, ssize_t max_split) {
    ssize_t last, next, count;
    last = 0;
    count = 0;

    while (count < max_split) {
        next = fastsearch(string + last, string_len - last, token, token_len, 1, FAST_SEARCH);
        if (next < 0) {
            break;
        } else {
            if (next == 0) {
                lua_pushstring(L, "");
            } else {
                lua_pushlstring(L, string + last, next);
            }
            last = last + next + token_len;
            lua_rawseti(L, -2, ++ count);
        }
    }
    if (last <= string_len) {  // edge case should add last empty string
        lua_pushlstring(L, string + last, string_len - last);
        lua_rawseti(L, -2, ++ count);
    }
    return count;
}

static int split(lua_State *L) {
    size_t string_len, token_len;
    const char *string = check_strict_string(L, 1, &string_len);

    if (string_len == 0) {
        lua_newtable(L);
        lua_pushstring(L, "");
        lua_rawseti(L, -2, 1);
        return 1;
    }
    if (lua_isstring(L, 2)) {  // split by token
        const char *token = check_strict_string(L, 2, &token_len);
        ssize_t max_split = string_len;
        if (lua_gettop(L) >= 3) {
            max_split = check_int(L, 3, string_len);
        }

        lua_newtable(L);
        if (token_len == 0) {
            lua_pushlstring(L, string, string_len);
            lua_rawseti(L, -2, 1);
            return 1;
        }
        if (max_split == 0) {
            lua_pushlstring(L, string, string_len);
            lua_rawseti(L, -2, 1);
            return 1;
        }

        max_split = max_split < 0 ? string_len : max_split;
        if (token_len == 1) {
            split_ch(L, string, string_len, token[0], max_split);
        } else {
            split_str(L, string, string_len, token, token_len, max_split);
        }
    } else if (lua_gettop(L) == 1 || lua_isnil(L, 2)) {  // split by nil
        token_len = 1;
        ssize_t max_split = luaL_optinteger(L, 3, -1);

        lua_newtable(L);
        if (max_split == 0) {
            lua_pushlstring(L, string, string_len);
            lua_rawseti(L, -2, 1);
            return 1;
        }
        max_split = max_split < 0 ? string_len : max_split;
        split_nil(L, string, string_len, max_split);
    } else {  //
        luaL_error(L, "split, invalid argument type: %s", luaL_typename(L, 2));
    }

    return 1;
}

static void _reverse_list(lua_State *L, ssize_t count, int index) {
    ssize_t i, j;
    for (i = 1, j = count; i < j; i++, j--) {
        lua_rawgeti(L, index, i);   // get i, top + 1
        lua_rawgeti(L, index, j);   // get j, top + 2

        lua_rawseti(L, index, i);   // push top + 2 to i
        lua_rawseti(L, index, j);   // push top + 1 to j
    }
}

static int rsplit_nil(lua_State *L, const char *string, size_t string_len, ssize_t max_split) {
    ssize_t i, last, count;
    last = string_len - 1;
    count = 0;

    i = last;
    if (is_nil_split(string[i])) {
        for (--i; i >= 0; i --) {
            last = i;
            if (!is_nil_split(string[i])) {
                break;
            }
        }
        if (i == 0) {
            return count;
        }
    }
    // string[i] is not nil_split
    for (i = last; i >= 0; i --) {
        if (is_nil_split(string[i])) {
            if (i < last) {
                lua_pushlstring(L, string + i + 1, last - i);
                lua_rawseti(L, -2, ++ count);
                last = i - 1;
                if (count == max_split) {
                    while (is_nil_split(string[last]) && last > 0) {
                        last --;
                    }
                    break;
                }
            } else {
                last = i - 1;
            }
        }
    }
    if (last >= 0) {
        lua_pushlstring(L, string, last + 1);
        lua_rawseti(L, -2, ++ count);
    }
    _reverse_list(L, count, lua_gettop(L));
    return count;
}

static int rsplit_ch(lua_State *L, const char *string, size_t string_len, char ch, ssize_t max_split) {
    ssize_t i, last, count;
    last = string_len - 1;
    count = 0;
    for (i = last; i >= 0; i --) {
        if (string[i] == ch) {
            if (last == i) {
                lua_pushstring(L, "");
            } else {
                lua_pushlstring(L, string + i + 1, last - i);
            }
            lua_rawseti(L, -2, ++ count);
            last = i - 1;
            if (count == max_split) {
                break;
            }
        }
    }
    if (last + 1 >= 0) {
        lua_pushlstring(L, string, last + 1);
        lua_rawseti(L, -2, ++ count);
    }
    _reverse_list(L, count, lua_gettop(L));
    return count;
}

static int rsplit_str(lua_State *L, const char *string, size_t string_len, const char *token, size_t token_len, ssize_t max_split) {
    ssize_t offset, next, count;
    offset = string_len;
    count = 0;

    while (count < max_split) {
        next = fastsearch(string, offset, token, token_len, 1, FAST_RSEARCH);
        if (next < 0) {
            break;
        } else {
            if (next + token_len == offset) {
                lua_pushstring(L, "");
            } else {
                lua_pushlstring(L, string + next + token_len, offset - next - token_len);
            }
            offset = next;
            lua_rawseti(L, -2, ++ count);
        }
    }
    if (offset >= 0) {
        lua_pushlstring(L, string, offset);
        lua_rawseti(L, -2, ++ count);
    }
    _reverse_list(L, count, lua_gettop(L));
    return count;
}

static int rsplit(lua_State *L) {
    size_t string_len, token_len;
    const char *string = check_strict_string(L, 1, &string_len);

    if (string_len == 0) {
        lua_newtable(L);
        lua_pushstring(L, "");
        lua_rawseti(L, -2, 1);
        return 1;
    }
    if (lua_isstring(L, 2)) {  // split by token
        const char *token = check_strict_string(L, 2, &token_len);
        ssize_t max_split = -1;
        if (lua_gettop(L) >= 3) {
            max_split = check_int(L, 3, string_len);
        }

        lua_newtable(L);
        if (token_len == 0) {
            lua_pushlstring(L, string, string_len);
            lua_rawseti(L, -2, 1);
            return 1;
        }
        if (max_split == 0) {
            lua_pushlstring(L, string, string_len);
            lua_rawseti(L, -2, 1);
            return 1;
        }

        max_split = max_split < 0 ? string_len : max_split;
        if (token_len == 1) {
            rsplit_ch(L, string, string_len, token[0], max_split);
        } else {
            rsplit_str(L, string, string_len, token, token_len, max_split);
        }
    } else if (lua_gettop(L) == 1 || lua_isnil(L, 2)) {  // split by nil
        token_len = 1;
        ssize_t max_split = luaL_optinteger(L, 3, -1);

        lua_newtable(L);
        if (max_split == 0) {
            lua_pushlstring(L, string, string_len);
            lua_rawseti(L, -2, 1);
            return 1;
        }
        max_split = max_split < 0 ? string_len : max_split;
        rsplit_nil(L, string, string_len, max_split);
    } else {  //
        luaL_error(L, "split, invalid argument type: %s", luaL_typename(L, 2));
    }

    return 1;
}

static int reverse_list(lua_State *L) {
    luaL_checktype(L, 1, LUA_TTABLE);
#if LUA_VERSION_NUM >=502
    ssize_t len = (L, 1);
#else
    ssize_t len = lua_objlen(L, 1);
#endif
    _reverse_list(L, len, lua_gettop(L));
    return 1;
}

static int join(lua_State *L) {
    size_t seq_len;
    const char *sep = check_strict_string(L, 1, &seq_len);
    luaL_checktype(L, 2, LUA_TTABLE);
    ssize_t i, len;

    size_t cell_len;
    const char *cell;

    len = lua_objlen(L, 2);
    luaL_Buffer buffer;
    luaL_buffinit(L, &buffer);
    if (len > 0) {
        for (i = 1; i < len; i ++) {
            lua_rawgeti(L, 2, i);
            cell = lua_tolstring(L, -1, &cell_len);
            luaL_addlstring(&buffer, cell, cell_len);
            if (seq_len > 0) {
                luaL_addlstring(&buffer, sep, seq_len);
            }
        }
        lua_rawgeti(L, 2, len);
        cell = lua_tolstring(L, -1, &cell_len);
        luaL_addlstring(&buffer, cell, cell_len);
    }
    luaL_pushresult(&buffer);
    return 1;
}

static int partition(lua_State *L) {
    size_t string_len, token_len;
    const char *string = check_strict_string(L, 1, &string_len);
    const char *token = check_strict_string(L, 2, &token_len);
    ssize_t pos = fastsearch(string, string_len, token, token_len, 1, FAST_SEARCH);
    if (pos < 0) {
        lua_pushstring(L, string);
        lua_pushstring(L, "");
        lua_pushstring(L, "");
    } else {
        lua_pushlstring(L, string, pos);
        lua_pushlstring(L, string + pos, token_len);
        lua_pushlstring(L, string + pos + token_len, string_len - pos - token_len);
    }
    return 3;
}

static int rpartition(lua_State *L) {
    size_t string_len, token_len;
    const char *string = check_strict_string(L, 1, &string_len);
    const char *token = check_strict_string(L, 2, &token_len);
    ssize_t pos = fastsearch(string, string_len, token, token_len, 1, FAST_RSEARCH);
    if (pos < 0) {
        lua_pushstring(L, "");
        lua_pushstring(L, "");
        lua_pushstring(L, string);
    } else {
        lua_pushlstring(L, string, pos);
        lua_pushlstring(L, string + pos, token_len);
        lua_pushlstring(L, string + pos + token_len, string_len - pos - token_len);
    }
    return 3;
}

static int check_nil_split(char ch, const char *token, size_t token_len) {
    return is_nil_split(ch);
}

static int check_is_token(char ch, const char *token, size_t token_len) {
    size_t i = 0;
    for (; i < token_len; i ++) {
        if (ch == token[i]) {
            return 1;  // found
        }
    }
    return 0;
}
typedef int (*check_func)(char ch, const char *token, size_t token_len);

static int lstrip(lua_State *L) {
    size_t string_len, token_len;
    const char *string = check_strict_string(L, 1, &string_len);
    check_func check;

    if (string_len == 0) {
        lua_pushstring(L, "");
        return 1;
    }

    const char * token;
    if (lua_gettop(L) == 1 || lua_isnil(L, 2)) {
        token = NULL;
        token_len = 0;
        check = check_nil_split;
    } else {
        token = check_strict_string(L, 2, &token_len);
        check = check_is_token;
    }

    ssize_t i;
    for (i = 0; i < string_len; i ++) {
        if (check(string[i], token, token_len) == 0) {
            break;
        }
    }
    if (i == string_len) {
        lua_pushstring(L, "");
    } else {
        lua_pushlstring(L, string + i, string_len - i);
    }
    return 1;
}

static int rstrip(lua_State *L) {
    size_t string_len, token_len;
    const char *string = check_strict_string(L, 1, &string_len);
    check_func check;

    if (string_len == 0) {
        lua_pushstring(L, "");
        return 1;
    }

    const char * token;
    if (lua_gettop(L) == 1||lua_isnil(L, 2)) {
        token = NULL;
        token_len = 0;
        check = check_nil_split;
    } else {
        token = check_strict_string(L, 2, &token_len);
        check = check_is_token;
    }

    ssize_t i;
    for (i = string_len - 1; i >= 0; i --) {
        if (check(string[i], token, token_len) == 0) {
            break;
        }
    }
    if (i < 0) {
        lua_pushstring(L, "");
    } else {
        lua_pushlstring(L, string, i + 1);
    }
    return 1;
}

static int strip(lua_State *L) {
    size_t string_len, token_len;
    const char *string = check_strict_string(L, 1, &string_len);
    check_func check;

    if (string_len == 0) {
        lua_pushstring(L, "");
        return 1;
    }

    const char * token;
    if (lua_gettop(L) == 1||lua_isnil(L, 2)) {
        token = NULL;
        token_len = 0;
        check = check_nil_split;
    } else {
        token = check_strict_string(L, 2, &token_len);
        check = check_is_token;
    }

    ssize_t i, j;
    for (i = 0; i < string_len; i ++) {
        if (check(string[i], token, token_len) == 0) {
            break;
        }
    }
    if (i == string_len) {
        lua_pushstring(L, "");
        return 1;
    }

    for (j = string_len - 1; j > i; j --) {
        if (check(string[j], token, token_len) == 0) {
            break;
        }
    }
    lua_pushlstring(L, string + i, j - i + 1);
    return 1;
}


#define MAP_FORMAT_KEY_INDEX    (-1)
#define MAP_FORMAT_VALUE_INDEX  (-2)
// format map to string,
// arg 1: format string, should be a string, user {k} to format key, {v} to format value
// arg 2: map, should be a table
// arg 3: sep, should be a string, default is ""
static int map_format(lua_State *L) {
    size_t string_len, seq_len;
    const char *string = check_strict_string(L, 1, &string_len);
    luaL_checktype(L, 2, LUA_TTABLE);

    const char *sep;
    if (lua_gettop(L) >= 3) {
        sep = check_strict_string(L, 3, &seq_len);
    } else {
        sep = "";
        seq_len = 0;
    }

    ssize_t i, last;
    int count;
    struct ut_nstr node;
    UT_array *strs;  // to record string.
    utarray_new(strs, &ut_nstr_icd);
    UT_array *indexs;  // to record index.
    utarray_new(indexs, &ut_int_icd);

    const int key_index = MAP_FORMAT_KEY_INDEX, value_index = MAP_FORMAT_VALUE_INDEX;
    i = 0; last = 0; count = 0;
    while (i < string_len) {
        char ch = string[i];
        if (ch == '{') {
            switch (string[i + 1]) {
            case 'k':
                if (string[i + 2] == '}') {
                    if (last < i) {
                        node.s = string + last;
                        node.len = i - last;
                        utarray_push_back(strs, &node);   // record string.
                        utarray_push_back(indexs, &count);  //record string index.
                        count ++;
                    }
                    utarray_push_back(indexs, &key_index);  // record key index.
                    i += 3;
                    last = i;
                } else {
                    utarray_free(strs);
                    utarray_free(indexs);
                    luaL_error(L, "map_format, invalid key argument: %s", string);
                }
                break;
            case 'v':
                if (string[i + 2] == '}') {
                    if (last < i) {
                        node.s = string + last;
                        node.len = i - last;
                        utarray_push_back(strs, &node);   // record string.
                        utarray_push_back(indexs, &count);  //record string index.
                        count ++;
                    }
                    utarray_push_back(indexs, &value_index);  // record key index.
                    i += 3;
                    last = i;
                } else {
                    utarray_free(strs);
                    utarray_free(indexs);
                    luaL_error(L, "map_format, invalid value argument: %s", string);
                }
                break;
            case '{':  // escape
                node.s = string + last;
                node.len = i - last + 1;          // contain "{"
                utarray_push_back(strs, &node);   // record string.
                utarray_push_back(indexs, &count);  //record string index.
                count ++;
                i += 2;
                last = i;
            default:
                utarray_free(strs);
                utarray_free(indexs);
                luaL_error(L, "map_format, invalid argument: %s", string);
            }
        }
        else if (ch == '}') {
            if (string[i + 1] == '}') {
                node.s = string + last;
                node.len = i - last + 1;          // contain "{"
                utarray_push_back(strs, &node);   // record string.
                utarray_push_back(indexs, &count);  //record string index.
                count ++;
                i += 2;
                last = i;
            } else {
                utarray_free(strs);
                utarray_free(indexs);
                luaL_error(L, "map_format, invalid argument: %s", string);
            }
        } else {
            i ++;
        }
    }

    if (last < string_len) {
        node.s = string + last;
        node.len = string_len - last;
        utarray_push_back(strs, &node);   // record string.
        utarray_push_back(indexs, &count);  //record string index.
    }

    int *p_index;
    char **p_str;

    count = 0;
    luaL_Buffer buffer;
    luaL_buffinit(L, &buffer);
    lua_pushnil(L);    // iterator for table arg 2
    while (lua_next(L, 2) != 0) {
        size_t key_len, value_len;
        const char *key = lua_tolstring(L, -2, &key_len);
        const char *value = lua_tolstring(L, -1, &value_len);
        lua_pop(L, 1);
        
        if (key == NULL || value == NULL) {
            utarray_free(strs);
            utarray_free(indexs);
            luaL_error(L, "map_format, invalid map key or value.");
        }

        if (count > 0 && seq_len > 0) {  // add for next string.
            luaL_addstring(&buffer, sep);
        }
        count ++;
        for (p_index = (int *)utarray_front(indexs); 
         p_index != NULL; 
         p_index = (int *)utarray_next(indexs, p_index)) {
            int index = *p_index;
            if (index == MAP_FORMAT_KEY_INDEX) {
                luaL_addstring(&buffer, key);
            } else if (index == MAP_FORMAT_VALUE_INDEX) {
                luaL_addstring(&buffer, value);
            } else {
                p_str = (char **)utarray_eltptr(strs, index);
                luaL_addstring(&buffer, *p_str);
            }
        }
    }

    utarray_free(indexs);
    utarray_free(strs);
    luaL_pushresult(&buffer);
    return 1;
}

static const luaL_Reg cpystring[] = {
    {"count", count},
    {"find", find},
    {"rfind", rfind},
    {"replace", replace},
    {"rreplace", rreplace},
    {"startswith", startswith},
    {"endswith", endswith},
    {"shift", shift},   // set
    {"swapcase", swapcase},
    {"split", split},
    {"rsplit", rsplit},
    {"center", center},
    {"ljust", ljust},
    {"rjust", rjust},
    {"reverse_list", reverse_list},
    {"join", join},
    {"partition", partition},
    {"rpartition", rpartition},
    {"lstrip", lstrip},
    {"rstrip", rstrip},
    {"strip", strip},
    {"map_format", map_format},
    {NULL, NULL}
};

int luaopen_cpystring(lua_State *L){
    luaL_newlib(L, cpystring);
    return 1;
}