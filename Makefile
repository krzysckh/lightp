.SUFFIXES: .scm .c
.PHONY: check-sqlite

OL=ol
CFLAGS=`pkg-config --cflags sqlite3`
LDFLAGS=-static `pkg-config --libs sqlite3` -lpthread -lm

all: check-sqlite lightp
check-sqlite:
	exit `$(OL) -e '(if (has? *features* (quote sqlite)) 0 1)'`
lightp: lightp.o
	$(CC) lightp.o -o $@ $(LDFLAGS)
.scm.c:
	$(OL) -x c -o $@ $<
clean:
	rm -f lightp
auto-install: all
	useradd -k /var/empty -L daemon -d /var/lightp -m -s /sbin/nologin _lightp || echo "user already exists. that's okay"
	cp -v lightp /var/lightp/lightp
	cp -v plot.pl /var/lightp/plot.pl
	chown _lightp:_lightp /var/lightp/lightp
	cp -v lightp.rc /etc/rc.d/lightp
	chmod +x /etc/rc.d/lightp
	chown _lightp:_lightp /dev/video0
