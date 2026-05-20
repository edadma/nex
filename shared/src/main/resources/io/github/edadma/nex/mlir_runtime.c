#include <math.h>
#include <stdio.h>
#include <stdint.h>

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

static void nex_fmt_f64(double v) {
    if (isnan(v)) { printf("nan"); return; }
    if (isinf(v)) { printf(v > 0 ? "inf" : "-inf"); return; }
    if (fabs(v) < 1e15 && v == (double)(long long)v) {
        printf("%lld.0", (long long)v);
        return;
    }
    printf("%.17g", v);
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
