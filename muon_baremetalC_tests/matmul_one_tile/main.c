// spin forever so we have a stable rv64 image

#include <stdio.h>
// mini_printf.c - tiny bare-metal printf using HTIF (tohost/fromhost)
#include "gemmini.h"
#include "matmul_data.h"
#include <stdint.h>
#include <stdarg.h>
#include <stddef.h>
#include <stdlib.h>

#define SYS_write 64

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

/* --- HTIF syscall wrapper --- */
static uintptr_t htif_syscall(uintptr_t which, uint64_t arg0,
                              uint64_t arg1, uint64_t arg2)
{
    volatile uint64_t magic_mem[8] __attribute__((aligned(64)));
    magic_mem[0] = which;
    magic_mem[1] = arg0;
    magic_mem[2] = arg1;
    magic_mem[3] = arg2;
    __sync_synchronize();

    tohost = (uintptr_t)magic_mem;
    while (fromhost == 0)
        ;
    fromhost = 0;

    __sync_synchronize();
    return magic_mem[0];
}

/* --- tiny string helpers (no libc) --- */
static size_t mini_strlen(const char *s)
{
    const char *p = s;
    while (*p) p++;
    return (size_t)(p - s);
}

static size_t mini_strnlen(const char *s, size_t n)
{
    const char *p = s;
    while (n-- && *p) p++;
    return (size_t)(p - s);
}

/* --- buffered putchar based on SYS_write --- */
static char outbuf[128];
static int  outlen = 0;

static void flush_buf(void)
{
    if (outlen > 0) {
        htif_syscall(SYS_write, 1, (uintptr_t)outbuf, (uint64_t)outlen);
        outlen = 0;
    }
}

static void raw_putc(int ch)
{
    if (outlen >= (int)sizeof(outbuf))
        flush_buf();
    outbuf[outlen++] = (char)ch;
    if (ch == '\n')
        flush_buf();
}

int putchar(int ch)
{
    raw_putc(ch);
    return ch;
}

void printstr(const char *s)
{
    size_t n = mini_strlen(s);
    htif_syscall(SYS_write, 1, (uintptr_t)s, (uint64_t)n);
}

/* --- number printing backend --- */
static void printnum(void (*putch)(int, void **), void **putdat,
                     unsigned long long num, unsigned base,
                     int width, int padc)
{
    unsigned digs[64];
    int pos = 0;

    if (base < 2) base = 10;

    do {
        digs[pos++] = num % base;
        num /= base;
    } while (num);

    while (width-- > pos)
        putch(padc, putdat);

    while (pos-- > 0) {
        unsigned d = digs[pos];
        char c = (d < 10) ? ('0' + d) : ('a' + (d - 10));
        putch(c, putdat);
    }
}

static unsigned long long getuint(va_list *ap, int lflag)
{
    if (lflag >= 2)
        return va_arg(*ap, unsigned long long);
    else if (lflag)
        return va_arg(*ap, unsigned long);
    else
        return va_arg(*ap, unsigned int);
}

static long long getint(va_list *ap, int lflag)
{
    if (lflag >= 2)
        return va_arg(*ap, long long);
    else if (lflag)
        return va_arg(*ap, long);
    else
        return va_arg(*ap, int);
}

/* putch adapter that just calls raw_putc */
static void printf_putch(int ch, void **data)
{
    (void)data;
    raw_putc(ch);
}

/* --- core formatter --- */
static void vprintfmt(void (*putch)(int, void **), void **putdat,
                      const char *fmt, va_list ap)
{
    const char *p;
    const char *last_fmt;
    int ch;
    unsigned long long num;
    int base, lflag, width, precision;
    int padc;

    for (;;)
    {
        // output non-% chars directly
        while ((ch = (unsigned char)*fmt) != '%' && ch != '\0') {
            fmt++;
            putch(ch, putdat);
        }
        if (ch == '\0')
            break;
        fmt++;  // skip '%'

        last_fmt = fmt;
        padc = ' ';
        width = -1;
        precision = -1;
        lflag = 0;

    reswitch:
        switch (ch = (unsigned char)*fmt++)
        {
        case '-':
            padc = '-';
            goto reswitch;

        case '0':
            padc = '0';
            goto reswitch;

        case '1': case '2': case '3': case '4':
        case '5': case '6': case '7': case '8':
        case '9':
            for (precision = 0;; ++fmt) {
                precision = precision * 10 + ch - '0';
                ch = *fmt;
                if (ch < '0' || ch > '9')
                    break;
            }
            if (width < 0) {
                width = precision;
                precision = -1;
            }
            goto reswitch;

        case '*':
            precision = va_arg(ap, int);
            if (width < 0) {
                width = precision;
                precision = -1;
            }
            goto reswitch;

        case '.':
            if (width < 0)
                width = 0;
            goto reswitch;

        case 'l':
            lflag++;
            goto reswitch;

        case 'c':
            putch(va_arg(ap, int), putdat);
            break;

        case 's':
            p = va_arg(ap, const char *);
            if (!p)
                p = "(null)";
            {
                int len = (int)mini_strnlen(p, (precision < 0) ? (size_t)-1 : (size_t)precision);
                if (width > 0 && padc != '-') {
                    while (width-- > len)
                        putch(padc, putdat);
                }
                for (int i = 0; i < len; i++)
                    putch(p[i], putdat);
                while (width-- > len)
                    putch(' ', putdat);
            }
            break;

        case 'd':
            num = getint(&ap, lflag);
            if ((long long)num < 0) {
                putch('-', putdat);
                num = -(long long)num;
            }
            base = 10;
            printnum(putch, putdat, num, base, width, padc);
            break;

        case 'u':
            base = 10;
            num = getuint(&ap, lflag);
            printnum(putch, putdat, num, base, width, padc);
            break;

        case 'x':
            base = 16;
            num = getuint(&ap, lflag);
            printnum(putch, putdat, num, base, width, padc);
            break;

        case 'p':
            // print pointer as 0x...
            putch('0', putdat);
            putch('x', putdat);
            base = 16;
            num = (uintptr_t)va_arg(ap, void *);
            printnum(putch, putdat, num, base, (width < 0 ? (int)(2 * sizeof(void *)) : width), '0');
            break;

        case '%':
            putch('%', putdat);
            break;

        default:
            // unknown, print literally
            putch('%', putdat);
            fmt = last_fmt;
            break;
        }
    }
}

/* --- public printf --- */
int printf(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    void *dummy = NULL;
    vprintfmt(printf_putch, &dummy, fmt, ap);
    va_end(ap);
    flush_buf();
    return 0; // not returning real length; good enough for bare-metal
}

#define SCALE_FACT_MEM 0x40088000


#define GEMMINI_CTRL 0x40084000
#define GEMMINI_RS1_ADDR (GEMMINI_CTRL + 0x10)
#define GEMMINI_RS2_ADDR (GEMMINI_CTRL + 0x18)
#define GEMMINI_INST_ADDR (GEMMINI_CTRL + 0x0)

#undef ROCC_INSTRUCTION_RS1_RS2
#define ROCC_INSTRUCTION_RS1_RS2(x, rs1, rs2, funct) { \
    *((volatile uint64_t *) GEMMINI_RS1_ADDR) = (rs1); \
    *((volatile uint64_t *) GEMMINI_RS2_ADDR) = (rs2); \
    *((volatile uint32_t*) GEMMINI_INST_ADDR) = (0x7B) | (0 << 7) | (3 << 12) | (1 << 15) | (2 << 20) | ((funct) << 25); \
}

#define k_MVIN         2
#define k_MVOUT        3
#define k_MVOUT_SPAD 23
#define XCUSTOM_ACC 3
#define ADDR_LEN 32
#define DIM 16

#undef elem_t
typedef uint16_t elem_t;    // A: 16b, packs 2× FP6(e3m2)
typedef uint32_t welem_t;   // B: 32b, packs 4× FP6(e3m2)
typedef uint64_t acc64_t;   // C: 64b, packs 4× BF16

void load_scale_factors(uint64_t* src, size_t bytes) {
  volatile uint64_t *dst = (volatile uint64_t *)SCALE_FACT_MEM;

  for (int transactions = 0; transactions < (bytes + 7) / 8; transactions++) {
    dst[transactions] = src[transactions];
  }
}

static inline void gemmini_init_ws(void) {
    // for some reason this is causing the test to hang

    // gemmini_config_ex(WEIGHT_STATIONARY, 0, 0);
    // gemmini_extended_config_st(DIM*sizeof(acc64_t), NO_ACTIVATION, 1);
}

#define A_SPAD      (0 * DIM)
#define B_SPAD      (1 * DIM)
#define C_ACC_SPAD  (1u << (ADDR_LEN - 1))

static void matmul_tile(const elem_t *A_packed,
                        const welem_t *B_packed,
                        acc64_t       *C_acc)
{
    // MVIN B (weights) as B^T for WS
    gemmini_config_ld(DIM * sizeof(welem_t));
    gemmini_mvin((uint64_t)B_packed, B_SPAD);

    // MVIN A (inputs)
    gemmini_config_ld(DIM * sizeof(elem_t));
    gemmini_mvin((uint64_t)A_packed, A_SPAD);

    // Preload + compute
    gemmini_config_ld(DIM * sizeof(welem_t));
    gemmini_preload(B_SPAD, C_ACC_SPAD);

    gemmini_config_ld(DIM * sizeof(elem_t));
    gemmini_compute_preloaded(A_SPAD, GARBAGE_ADDR);

    // MVOUT C
    gemmini_mvout((uint64_t)C_acc, C_ACC_SPAD);

    // Make sure everything is done
    gemmini_fence();
}


int main(void) {

  printf("Initializing Gemmini in WS mode...\n");

  gemmini_flush(0);
  gemmini_init_ws();

  // Output buffer in DRAM for one DIM x DIM tile
  static acc64_t C_acc[DIM * DIM];
  uint8_t* result = (uint8_t*) C_acc;

  printf("Loading scale factors...\n");
  load_scale_factors((uint64_t*) C_scale, sizeof(C_scale));
  printf("Finished loading scale factors.\n");

  printf("Running 1-tile matmul (DIM x DIM)...\n");
  matmul_tile(A_in, B_in, C_acc);
  printf("Matmul done. Dumping C_acc (hex):\n");

//   for (int i = 0; i < DIM; i++) {
//     for (int j = 0; j < DIM; j++) {
//       uint8_t v = result[i * DIM + j];
//       printf("%d ", v);
//     }
//     printf("\n");
//   }

  printf("Done.\n");
  exit(0);
}

