#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

/* Runtime traps. These mirror the LLVM-backend trap messages — the
 * MLIR backend has no setjmp/longjmp / `assert_traps` machinery yet, so
 * a trap prints to stdout (where parity tests compare) and exits with
 * status 1. The message text matches `NexLLVMPreamble`'s `slice_oob_msg`
 * and `axis_oob_msg` byte-for-byte.
 */
void nex_trap_slice_oob(void) {
    printf("trap: slice out of bounds\n");
    exit(1);
}

void nex_trap_axis_oob(void) {
    printf("trap: axis index out of bounds\n");
    exit(1);
}

void nex_trap_complex_div_zero(void) {
    printf("trap: complex division by zero\n");
    exit(1);
}

/* ---------------------------------------------------------------------------
 * Scalar formatting helpers used by both scalar print and the per-element
 * paths in the array printers. The trailing-newline variants follow
 * NexInterpreter.formatValue: integers print as-is, reals get a whole-number
 * `<int>.0` form when |v| < 1e15 and v == trunc(v), NaN/Inf get the
 * interpreter spellings, anything else falls back to %.17g — a documented
 * divergence from Scala's shortest-round-trip Double.toString until we ship
 * a Ryu / Grisu-equivalent helper.
 *
 * The element formatters (nex_fmt_i64 / nex_fmt_f64) write the SAME content
 * minus the newline so the array printers can compose them with brackets
 * and commas without re-implementing the rules.
 */

static void nex_fmt_i64(int64_t v) {
    printf("%lld", (long long)v);
}

/* Java-style real post-processing: rewrites a `%g`-produced buffer's
 * `e±XX` exponent into Java's `E[±]N` form (uppercase E, no leading
 * `+`, leading zeros stripped except one), inserting `.0` before the
 * `E` if the mantissa lacks a `.`. Buffers without `e` pass through
 * unchanged. Mirrors LLVM's `__nex_format_real_java`.
 */
static void nex_format_real_java(const char* in, char* out) {
    const char* e = strchr(in, 'e');
    if (!e) {
        strcpy(out, in);
        return;
    }
    size_t mlen = (size_t)(e - in);
    memcpy(out, in, mlen);
    char* op = out + mlen;
    if (!memchr(in, '.', mlen)) {
        *op++ = '.';
        *op++ = '0';
    }
    *op++ = 'E';
    const char* ep = e + 1;
    if (*ep == '+') {
        ep++;
    } else if (*ep == '-') {
        *op++ = '-';
        ep++;
    }
    while (*ep == '0' && *(ep + 1) >= '0' && *(ep + 1) <= '9') {
        ep++;
    }
    strcpy(op, ep);
}

/* Shortest-round-trip f64 formatter shared by `print(x)`, the array
 * printers, and `nex_str_from_f64`. NaN / ±inf direct; whole-int fast
 * path; otherwise `%.<p>g` for p=1..17 picks the smallest p whose
 * output strtod re-parses bit-equal to v, then Java post-process to
 * match LLVM byte-for-byte.
 */
static void nex_fmt_f64(double v) {
    if (isnan(v)) { printf("nan"); return; }
    if (isinf(v)) { printf(v > 0 ? "inf" : "-inf"); return; }
    if (fabs(v) < 1e15 && v == (double)(long long)v) {
        printf("%lld.0", (long long)v);
        return;
    }
    char tmp[40];
    char out[48];
    char fmt[8];
    int p;
    for (p = 1; p <= 17; p++) {
        snprintf(fmt, sizeof(fmt), "%%.%dg", p);
        snprintf(tmp, sizeof(tmp), fmt, v);
        if (strtod(tmp, NULL) == v) break;
    }
    nex_format_real_java(tmp, out);
    fputs(out, stdout);
}

void nex_print_i64(int64_t v) {
    nex_fmt_i64(v);
    putchar('\n');
}

void nex_print_f64(double v) {
    nex_fmt_f64(v);
    putchar('\n');
}

/* Bool printer. Lowering passes the value as `i1`; clang ABI-promotes
 * it to a full int, so a plain `_Bool` parameter is portable enough.
 * Output matches NexInterpreter.formatValue: lowercase "true"/"false".
 */
void nex_print_bool(_Bool v) {
    printf(v ? "true\n" : "false\n");
}

/* Integer exponentiation by squaring, mirroring NexLLVMPreamble's
 * @__nex_ipow. Result = base^exp for exp >= 0 (spec §4.4); returns 0
 * defensively for negative exponents — the elaborator forces a
 * TyReal result type for those, so the integer path never reaches
 * a negative exponent in well-typed programs.
 */
int64_t nex_ipow(int64_t base, int64_t exp) {
    if (exp < 0) return 0;
    int64_t result = 1;
    int64_t b = base;
    int64_t e = exp;
    while (e > 0) {
        if (e & 1) result *= b;
        e >>= 1;
        if (e > 0) b *= b;
    }
    return result;
}

/* ---------------------------------------------------------------------------
 * Array printers. The MLIR side passes the aligned data pointer as an
 * intptr_t-shaped i64 (from `memref.extract_aligned_pointer_as_index` +
 * `arith.index_castui`) plus the dimensions as i64 constants. Layout is
 * row-major for rank-2, matching NexInterpreter.VArray2's `b(i * cols + j)`.
 * Output matches NexInterpreter.formatValue's bracketed comma format:
 *   rank-1: `[1, 2, 3]\n`
 *   rank-2: `[[1, 2], [3, 4]]\n`
 */

void nex_print_array_1d_i64(int64_t ptr_as_int, int64_t len) {
    int64_t* buf = (int64_t*)(intptr_t)ptr_as_int;
    putchar('[');
    for (int64_t i = 0; i < len; i++) {
        if (i > 0) printf(", ");
        nex_fmt_i64(buf[i]);
    }
    printf("]\n");
}

void nex_print_array_1d_f64(int64_t ptr_as_int, int64_t len) {
    double* buf = (double*)(intptr_t)ptr_as_int;
    putchar('[');
    for (int64_t i = 0; i < len; i++) {
        if (i > 0) printf(", ");
        nex_fmt_f64(buf[i]);
    }
    printf("]\n");
}

void nex_print_array_2d_i64(int64_t ptr_as_int, int64_t rows, int64_t cols) {
    int64_t* buf = (int64_t*)(intptr_t)ptr_as_int;
    putchar('[');
    for (int64_t i = 0; i < rows; i++) {
        if (i > 0) printf(", ");
        putchar('[');
        for (int64_t j = 0; j < cols; j++) {
            if (j > 0) printf(", ");
            nex_fmt_i64(buf[i * cols + j]);
        }
        putchar(']');
    }
    printf("]\n");
}

void nex_print_array_2d_f64(int64_t ptr_as_int, int64_t rows, int64_t cols) {
    double* buf = (double*)(intptr_t)ptr_as_int;
    putchar('[');
    for (int64_t i = 0; i < rows; i++) {
        if (i > 0) printf(", ");
        putchar('[');
        for (int64_t j = 0; j < cols; j++) {
            if (j > 0) printf(", ");
            nex_fmt_f64(buf[i * cols + j]);
        }
        putchar(']');
    }
    printf("]\n");
}

/* Bool array printers. MLIR's `tensor<NxI1>` lowers to a memref of i8 in
 * LLVM dialect (one byte per packed-out element), so the data pointer
 * is byte-strided. Each element is zero/non-zero in the low bit.
 */

static void nex_fmt_bool(int8_t v) {
    printf(v ? "true" : "false");
}

void nex_print_array_1d_bool(int64_t ptr_as_int, int64_t len) {
    int8_t* buf = (int8_t*)(intptr_t)ptr_as_int;
    putchar('[');
    for (int64_t i = 0; i < len; i++) {
        if (i > 0) printf(", ");
        nex_fmt_bool(buf[i]);
    }
    printf("]\n");
}

void nex_print_array_2d_bool(int64_t ptr_as_int, int64_t rows, int64_t cols) {
    int8_t* buf = (int8_t*)(intptr_t)ptr_as_int;
    putchar('[');
    for (int64_t i = 0; i < rows; i++) {
        if (i > 0) printf(", ");
        putchar('[');
        for (int64_t j = 0; j < cols; j++) {
            if (j > 0) printf(", ");
            nex_fmt_bool(buf[i * cols + j]);
        }
        putchar(']');
    }
    printf("]\n");
}

/* ---------------------------------------------------------------------------
 * Strings. Mirrors the LLVM backend's %nex_str descriptor — three fields:
 * refcount, length (in bytes, NOT counting any trailing NUL), and an opaque
 * data pointer. refcount == -1 marks the descriptor immortal — both inc and
 * dec are no-ops for those. Heap descriptors (concat results, value-to-string
 * conversions) start at refcount 1 and free both descriptor and data when
 * refcount hits 0.
 *
 * String literals live in the binary as module-level `memref.global` byte
 * arrays; `nex_str_lit_from_cstr` builds (and caches) one immortal descriptor
 * per literal address on first use. Identity by data-pointer is the key —
 * the MLIR codegen dedups literal text into one `memref.global` per unique
 * text, so two source-level `"hello"`s share one entry here too.
 *
 * The MLIR side passes descriptor pointers as int64-shaped i64 values
 * (intptr_t round-tripped through arith.index_castui). All string ops
 * are runtime calls — the codegen never derefs a descriptor itself.
 */

typedef struct nex_str {
    int64_t refcount;
    int64_t length;
    const char* data;
} nex_str;

#define NEX_STR_LIT_CACHE_MAX 4096
static nex_str   nex_str_lit_cache[NEX_STR_LIT_CACHE_MAX];
static const char* nex_str_lit_keys[NEX_STR_LIT_CACHE_MAX];
static int       nex_str_lit_count = 0;

int64_t nex_str_lit_from_cstr(int64_t data_ptr, int64_t len) {
    const char* data = (const char*)(intptr_t)data_ptr;
    for (int i = 0; i < nex_str_lit_count; i++) {
        if (nex_str_lit_keys[i] == data) {
            return (int64_t)(intptr_t)&nex_str_lit_cache[i];
        }
    }
    if (nex_str_lit_count >= NEX_STR_LIT_CACHE_MAX) {
        fprintf(stderr, "nex_str_lit cache overflow\n");
        exit(1);
    }
    int idx = nex_str_lit_count++;
    nex_str_lit_keys[idx] = data;
    nex_str_lit_cache[idx].refcount = -1;
    nex_str_lit_cache[idx].length   = len;
    nex_str_lit_cache[idx].data     = data;
    return (int64_t)(intptr_t)&nex_str_lit_cache[idx];
}

void nex_str_inc(int64_t p) {
    nex_str* s = (nex_str*)(intptr_t)p;
    if (s->refcount == -1) return;
    s->refcount++;
}

void nex_str_dec(int64_t p) {
    nex_str* s = (nex_str*)(intptr_t)p;
    if (s->refcount == -1) return;
    if (--s->refcount == 0) {
        free((void*)s->data);
        free(s);
    }
}

void nex_print_str(int64_t p) {
    nex_str* s = (nex_str*)(intptr_t)p;
    fwrite(s->data, 1, (size_t)s->length, stdout);
    putchar('\n');
}

/* Concatenate two strings into a fresh heap descriptor (refcount=1).
 * Does NOT decrement either operand — callers must release them as
 * part of normal refcount discipline. Mirrors LLVM's
 * __nex_str_concat semantics.
 */
int64_t nex_str_concat(int64_t pa, int64_t pb) {
    nex_str* a = (nex_str*)(intptr_t)pa;
    nex_str* b = (nex_str*)(intptr_t)pb;
    int64_t total = a->length + b->length;
    char* buf = (char*)malloc((size_t)total + 1);
    if (a->length > 0) memcpy(buf, a->data, (size_t)a->length);
    if (b->length > 0) memcpy(buf + a->length, b->data, (size_t)b->length);
    buf[total] = '\0';
    nex_str* out = (nex_str*)malloc(sizeof(nex_str));
    out->refcount = 1;
    out->length   = total;
    out->data     = buf;
    return (int64_t)(intptr_t)out;
}

/* Byte-wise string equality. Returns 1 if both descriptors carry the
 * same byte content (length first, then memcmp), 0 otherwise. The MLIR
 * codegen widens this to `!=` by emitting an `arith.xori` against 1.
 */
int8_t nex_str_eq(int64_t pa, int64_t pb) {
    nex_str* a = (nex_str*)(intptr_t)pa;
    nex_str* b = (nex_str*)(intptr_t)pb;
    if (a == b) return 1;
    if (a->length != b->length) return 0;
    if (a->length == 0) return 1;
    return memcmp(a->data, b->data, (size_t)a->length) == 0 ? 1 : 0;
}

/* Allocate a fresh heap descriptor with `len` bytes of content. The
 * caller fills `data` directly. Used by every value-to-string builder
 * below.
 */
static nex_str* nex_str_alloc_descriptor(int64_t len, char* data) {
    nex_str* s = (nex_str*)malloc(sizeof(nex_str));
    s->refcount = 1;
    s->length   = len;
    s->data     = data;
    return s;
}

/* i64 → fresh heap descriptor (refcount=1) carrying the decimal
 * representation. Matches NexInterpreter.formatValue / LLVM
 * __nex_str_from_i64 byte-for-byte.
 */
int64_t nex_str_from_i64(int64_t v) {
    char tmp[32];
    int len = snprintf(tmp, sizeof(tmp), "%lld", (long long)v);
    char* buf = (char*)malloc((size_t)len + 1);
    memcpy(buf, tmp, (size_t)len + 1);
    return (int64_t)(intptr_t)nex_str_alloc_descriptor(len, buf);
}

/* i1 (passed in low bit of an int8_t) → fresh heap descriptor.
 * Returns "true" or "false" — same spelling as the interpreter and
 * LLVM backend. Heap-allocated so the descriptor follows the same
 * dec-when-consumed discipline as every other value-to-string output.
 */
int64_t nex_str_from_bool(int8_t v) {
    const char* text = v ? "true" : "false";
    int len = v ? 4 : 5;
    char* buf = (char*)malloc((size_t)len + 1);
    memcpy(buf, text, (size_t)len + 1);
    return (int64_t)(intptr_t)nex_str_alloc_descriptor(len, buf);
}

/* Double → fresh heap descriptor. Matches the LLVM backend's
 * `__nex_str_from_double` byte-for-byte:
 *
 *   - NaN  → "nan"
 *   - +inf → "inf", -inf → "-inf"
 *   - whole-int magnitude with |v| < 1e15 → "<lld>.0" (fast path)
 *   - otherwise: shortest-round-trip via %.1g..%.17g (first p whose
 *     output strtod's bit-equal to v), then Java post-process to
 *     convert `1e-20` → `1.0E-20`, `1.5e+20` → `1.5E20`, etc.
 */
int64_t nex_str_from_f64(double v) {
    char out[48];
    if (isnan(v)) {
        strcpy(out, "nan");
    } else if (isinf(v)) {
        strcpy(out, v > 0 ? "inf" : "-inf");
    } else if (fabs(v) < 1e15 && v == (double)(long long)v) {
        snprintf(out, sizeof(out), "%lld.0", (long long)v);
    } else {
        char tmp[40];
        char fmt[8];
        int p;
        for (p = 1; p <= 17; p++) {
            snprintf(fmt, sizeof(fmt), "%%.%dg", p);
            snprintf(tmp, sizeof(tmp), fmt, v);
            if (strtod(tmp, NULL) == v) break;
        }
        nex_format_real_java(tmp, out);
    }
    int len = (int)strlen(out);
    char* buf = (char*)malloc((size_t)len + 1);
    memcpy(buf, out, (size_t)len + 1);
    return (int64_t)(intptr_t)nex_str_alloc_descriptor(len, buf);
}

/* Integer format spec helper. Spec is a `%[flags][width][.prec]<conv>`
 * string passed as (ptr, len) — the codegen interns it as a literal
 * byte array. Conversion char is one of `d x X o`. We translate the
 * spec into a C int64 spec by inserting `ll` before the conversion
 * char (so `%5d` → `%5lld` etc.), then snprintf into a stack buffer,
 * then heap-allocate a fresh descriptor.
 */
int64_t nex_str_format_i64(int64_t spec_ptr, int64_t spec_len, int64_t value) {
    char spec[32], cfmt[40];
    if (spec_len <= 0 || spec_len >= (int64_t)sizeof(spec)) {
        return nex_str_from_i64(value);
    }
    memcpy(spec, (const char*)(intptr_t)spec_ptr, (size_t)spec_len);
    spec[spec_len] = '\0';
    int len = (int)spec_len;
    // Copy spec[0..len-1] into cfmt, then insert "ll" before the
    // conversion char (last byte) and re-emit it.
    int j = 0;
    for (int i = 0; i < len - 1 && j < (int)sizeof(cfmt) - 5; i++) {
        cfmt[j++] = spec[i];
    }
    cfmt[j++] = 'l';
    cfmt[j++] = 'l';
    cfmt[j++] = spec[len - 1];
    cfmt[j]   = '\0';
    char tmp[80];
    int n = snprintf(tmp, sizeof(tmp), cfmt, (long long)value);
    if (n < 0) n = 0;
    if (n >= (int)sizeof(tmp)) n = (int)sizeof(tmp) - 1;
    char* buf = (char*)malloc((size_t)n + 1);
    memcpy(buf, tmp, (size_t)n + 1);
    return (int64_t)(intptr_t)nex_str_alloc_descriptor(n, buf);
}

/* Real format spec helper. Spec is a `%[flags][width][.prec]<conv>`
 * string passed as (ptr, len). Conversion chars `f e g E G` all take
 * a C `double`, so the spec passes through unmodified.
 */
int64_t nex_str_format_f64(int64_t spec_ptr, int64_t spec_len, double value) {
    char spec[32];
    if (spec_len <= 0 || spec_len >= (int64_t)sizeof(spec)) {
        return nex_str_from_i64((int64_t)value);
    }
    memcpy(spec, (const char*)(intptr_t)spec_ptr, (size_t)spec_len);
    spec[spec_len] = '\0';
    char tmp[80];
    int n = snprintf(tmp, sizeof(tmp), spec, value);
    if (n < 0) n = 0;
    if (n >= (int)sizeof(tmp)) n = (int)sizeof(tmp) - 1;
    char* buf = (char*)malloc((size_t)n + 1);
    memcpy(buf, tmp, (size_t)n + 1);
    return (int64_t)(intptr_t)nex_str_alloc_descriptor(n, buf);
}

/* Binary format spec helper. C printf doesn't have `%b`, so we build
 * the digits MSB-first by shifting `value` (unsigned), then pad on
 * either side per the spec's flags. Width comes pre-parsed from the
 * codegen; zero_pad and left_align are 0/1 i8 flags. Output mirrors
 * the LLVM backend's `__nex_print_real`-adjacent binary path
 * byte-for-byte: leading zeros when `0`-flag set, spaces otherwise,
 * left-justify when `-`-flag set.
 */
int64_t nex_str_format_bin(int64_t value, int64_t width, int8_t zero_pad, int8_t left_align) {
    char digits[65];
    int dlen = 0;
    uint64_t u = (uint64_t)value;
    if (u == 0) {
        digits[dlen++] = '0';
    } else {
        char rev[65];
        int rlen = 0;
        while (u != 0) {
            rev[rlen++] = (u & 1u) ? '1' : '0';
            u >>= 1;
        }
        for (int i = rlen - 1; i >= 0; i--) digits[dlen++] = rev[i];
    }
    int total = (width > (int64_t)dlen) ? (int)width : dlen;
    char* buf = (char*)malloc((size_t)total + 1);
    if (left_align) {
        memcpy(buf, digits, (size_t)dlen);
        for (int i = dlen; i < total; i++) buf[i] = ' ';
    } else {
        char pad = zero_pad ? '0' : ' ';
        for (int i = 0; i < total - dlen; i++) buf[i] = pad;
        memcpy(buf + (total - dlen), digits, (size_t)dlen);
    }
    buf[total] = '\0';
    return (int64_t)(intptr_t)nex_str_alloc_descriptor(total, buf);
}

// ---------------------------------------------------------------------------
// Closures (escaping lambdas).
//
// A closure value is the i64 address of a heap descriptor
//   { int64_t fn_ptr; int64_t env_ptr; }
// where fn_ptr is the address of the synthetic `nex_lambda_<N>` function
// (taken in MLIR via `llvm.mlir.addressof` + `llvm.ptrtoint`) and env_ptr
// is the address of a heap-allocated env blob.
//
// Each lambda's env is a flat byte buffer: 8 bytes per capture slot. ByRef
// captures store a pointer to a separately heap-allocated "var box" (also
// an 8-byte cell holding the var's current value); ByVal captures store
// the captured value directly. The codegen sees env / boxes through plain
// load_i64 / store_i64 — type information is per-slot at the codegen
// level only.
//
// Refcounting is NOT YET implemented; envs, boxes, and descriptors leak
// for the duration of the process. Test programs are short-running so
// the leak is acceptable for the first slice; refcount infrastructure
// lands when the closure surface expands (see roadmap memo).
// ---------------------------------------------------------------------------

int64_t nex_env_alloc(int64_t size) {
    void *p = calloc(1, (size_t)size);
    return (int64_t)(intptr_t)p;
}

int64_t nex_env_load_i64(int64_t env, int64_t offset) {
    int64_t v;
    memcpy(&v, (char *)(intptr_t)env + offset, sizeof(int64_t));
    return v;
}

void nex_env_store_i64(int64_t env, int64_t offset, int64_t value) {
    memcpy((char *)(intptr_t)env + offset, &value, sizeof(int64_t));
}

typedef struct {
    int64_t fn_ptr;
    int64_t env_ptr;
} nex_closure;

int64_t nex_closure_make(int64_t fn_ptr, int64_t env) {
    nex_closure *cl = (nex_closure *)malloc(sizeof(nex_closure));
    cl->fn_ptr = fn_ptr;
    cl->env_ptr = env;
    return (int64_t)(intptr_t)cl;
}

int64_t nex_closure_fn(int64_t closure) {
    return ((nex_closure *)(intptr_t)closure)->fn_ptr;
}

int64_t nex_closure_env(int64_t closure) {
    return ((nex_closure *)(intptr_t)closure)->env_ptr;
}
