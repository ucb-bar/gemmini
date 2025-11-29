
#include <stdio.h>

#include <stdint.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>

#define SCALE_FACT_MEM 0x40088000

volatile unsigned long sink;
int main(void) {

  const uint32_t src[] = { 0x11223344, 0x55667788, 0xDEADBEEF, 0x11223344 };
  size_t bytes = 16;

  volatile uint64_t *dst = (volatile uint64_t *)SCALE_FACT_MEM;
  const uint64_t *s = (const uint64_t *)src;
  for (size_t i = 0; i < bytes/8; i++) dst[i] = s[i];

  exit(0);
}

