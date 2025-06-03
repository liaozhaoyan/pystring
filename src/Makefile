LUA ?= lua5.1
LUA_PC ?= lua5.1
LUA_CFLAGS = $(shell pkg-config $(LUA_PC) --cflags)
LUA_LIB = cpystring

CFLAGS ?= -O3 -g -fPIC

all: $(LUA_LIB).so

%.o: %.c
	$(CC) -c $(CFLAGS) $(LUA_CFLAGS) -o $@ $<

$(LUA_LIB).so: $(LUA_LIB).o
	$(CC) -shared $(LUA_LIB).o -o $@

clean:
	rm -f $(LUA_LIB).so *.o *.rock