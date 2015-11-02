PKGCONFIG = pkg-config

AM_CFLAGS = -std=gnu99 -fPIC
CFLAGS = -D_FORTIFY_SOURCE=2 -Wall -W -O2

AM_LDFLAGS = -fPIC
LDFLAGS = -Wl,-as-needed

lua_CFLAGS = $(shell ${PKGCONFIG} --cflags lua x11 xscrnsaver)
lua_LIBS   = $(shell ${PKGCONFIG} --libs lua x11 xscrnsaver)

all:	x11.so

x11.so:	x11.o
	$(CC) $(AM_LDFLAGS) $(LDFLAGS) -shared $^ -o $@ $(LIBS) $(lua_LIBS)

%.o:	%.c
	$(CC) $(AM_CFLAGS) $(lua_CFLAGS) $(CFLAGS) $< -o $@ -c
