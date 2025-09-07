.SUFFIXES: .scm .c

OL=ol
CFLAGS != $(OL) -e '(if (has? *features* (quote sqlite)) "`pkg-config --cflags --libs sqlite3`" "")'
CFLAGS += -static -lm -lpthread

all: lightp
.scm.c:
	$(OL) -x c -o $@ $<
clean:
	rm -f lightp
