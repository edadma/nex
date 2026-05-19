#include <math.h>
#include <stdio.h>
#include <stdint.h>

void nex_print_i64(int64_t v) {
    printf("%lld\n", (long long)v);
}

/* Mirror NexInterpreter.formatValue for VReal:
 *   NaN     -> "nan"
 *   +/-Inf  -> "inf" / "-inf"
 *   whole   -> "<int>.0"   (when |v| < 1e15 AND v == trunc(v))
 *   else    -> Scala-style shortest round-trip Double.toString
 *
 * Achieving Scala's shortest-round-trip print in C from scratch is a
 * project (Ryu / Grisu). For now we only need to match the interpreter
 * exactly on values reachable from the milestone-2 source surface
 * (whole-number reals from literal sums). Non-whole reals fall back to
 * %.17g, which preserves round-trip but does NOT byte-match the
 * interpreter — that gap will close when we ship a real shortest-print
 * helper.
 */
void nex_print_f64(double v) {
    if (isnan(v)) {
        printf("nan\n");
        return;
    }
    if (isinf(v)) {
        printf(v > 0 ? "inf\n" : "-inf\n");
        return;
    }
    if (fabs(v) < 1e15 && v == (double)(long long)v) {
        printf("%lld.0\n", (long long)v);
        return;
    }
    printf("%.17g\n", v);
}
