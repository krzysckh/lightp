#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <err.h>
#include <errno.h>

#include <ovm.h>

#define WIDTH 160
#define HEIGHT 120
#define bufsize (WIDTH*HEIGHT*2)

#define CMD "video -e yuy2 -r 1 -s %dx%d -f %s -o -"

#define USED(x) ((void)(x))

pthread_t thr;

pthread_mutex_t mem_lock;
uint8_t *mem = NULL;

void
forceread(uint8_t *buf, long n, int fd)
{
  ssize_t rd = 0;
  while (rd < (ssize_t)n) {
    rd = read(fd, buf, n);
    buf += rd, n -= rd;
  }
}

void *
run_video_stream(void *dev_) {
  char *dev = dev_, buf[512] = {0}; /* if your device is longer than that, you're wrong */
  uint8_t temp_buf[bufsize];
  int fds[2];
  fd_set rs, ws, es;
  snprintf(buf, 512, CMD, WIDTH, HEIGHT, dev);

  if (pipe(fds) < 0)
    err(errno, "pipe");

  if (fork() == 0) {
    dup2(fds[1], STDOUT_FILENO);
    execl("/bin/sh", "sh", "-c", buf, NULL); /* unsafe. lol */
    exit(0);
  }

  while (1) {
    FD_ZERO(&rs); FD_ZERO(&ws); FD_ZERO(&es);
    FD_SET(fds[0], &rs);
    select(fds[0]+1, &rs, &ws, &es, NULL);
    forceread(temp_buf, bufsize, fds[0]);

    pthread_mutex_lock(&mem_lock);
    memcpy(mem, temp_buf, bufsize);
    pthread_mutex_unlock(&mem_lock);
  }

  return NULL;
}

word
prim_custom(int op, word a, word b, word c)
{
  USED(b); USED(c);
  switch (op) {
  case 1000: /* initialize dev -> #t | #f*/
    printf("In init...\n");
    pthread_mutex_init(&mem_lock, NULL);
    mem = malloc(bufsize);
    return pthread_create(&thr, NULL, run_video_stream, cstr(a)) == 0 ? ITRUE : IFALSE;
  case 1001: { /* query -> avg */
    size_t i = 0;
    uint64_t d = 0;
    pthread_mutex_lock(&mem_lock);
    for (i = 0; i < bufsize; i += 2)
      d += mem[i]&0xf0;
    pthread_mutex_unlock(&mem_lock);
    return mkrat(d, bufsize/2);
  }
  }

  return IFALSE;
}
