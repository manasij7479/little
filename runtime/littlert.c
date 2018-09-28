#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#define __STDC_FORMAT_MACROS
#include <inttypes.h>

extern int l_main(void);
int main() {
  return l_main();
}

int64_t input() {
  int64_t n;
  scanf("%"SCNi64, &n);
  return n;
}

void printint(int64_t x) {
  printf("%"PRIi64, x);
}

void printstring(char* str) {
  printf("%s", str);
}

void l_abort(char* str) {
  perror(str);
  abort();
}

uint64_t* heapalloc(uint64_t size) {
  uint64_t* result = malloc(size * sizeof(uint64_t));
  if (!result) {
    l_abort("heapalloc failed");
  }
  return result;
}

void heapfree(uint64_t* mem) {
  free(mem);
}

uint64_t l_exp(uint64_t x, uint64_t n) {
  uint64_t result = 1;
  while (n--) {
    result *= x;
  }
  return result;
}
