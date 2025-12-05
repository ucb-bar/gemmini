
#include <stdio.h>

#include <stdint.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>

extern volatile uint64_t tohost;
extern volatile uint64_t fromhost;

void __attribute__((noreturn)) tohost_exit(uintptr_t code)
{
  tohost = (code << 1) | 1;
  while (1);
}

void exit(int code)
{
  tohost_exit(code);
}


#define SCALE_FACT_MEM 0x40088000

int main(void) {

  const uint32_t src[] = { 0x11223344, 0x55667788, 0xDEADBEEF, 0x11223344 };
  size_t bytes = 16;

  volatile uint64_t *dst = (volatile uint64_t *)SCALE_FACT_MEM;
  const uint64_t *s = (const uint64_t *)src;
  for (size_t i = 0; i < bytes/8; i++) dst[i] = s[i];

  exit(0);
}

