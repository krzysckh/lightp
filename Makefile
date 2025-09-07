.SUFFIXES: .scm .c

OL=ol
CFLAGS != $(OL) -e '(if (has? *features* (quote sqlite)) "`pkg-config --cflags --libs sqlite3`" "")'
CFLAGS += -static -lm -lpthread

all: lightp
.scm.c:
	$(OL) -x c -o $@ $<
clean:
	rm -f lightp
auto-install: all
	useradd -k /var/empty -L daemon -d /var/lightp -m -s /sbin/nologin _lightp || echo "user already exists. that's okay"
	cp -v lightp /var/lightp/lightp
	chown _lightp:_lightp /var/lightp/lightp
	cp -v lightp.rc /etc/rc.d/lightp
	chmod +x /etc/rc.d/lightp
	chown _lightp:_lightp /dev/video0
