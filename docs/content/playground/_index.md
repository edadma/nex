---
title: Playground
summary: Run Nex code in the browser. No install, no setup.
weight: 25
hideFromHome: true
---

The playground runs the same tree-walking interpreter that powers `nex run` — compiled to JavaScript via Scala.js and loaded lazily into the page on first click. Single-file programs only; the playground bypasses the module loader so there's no filesystem dependency, no Node assumption.

Click **Run** to execute. The first run loads the interpreter bundle (~1.8 MB minified, ~400 KB over compression), so it takes a second; subsequent runs reuse the cached bundle and finish in milliseconds.

## Hello

[= playground =]
def main() =
  print("Hello from Nex")
  print(1 + 2 * 3)
[= /playground =]

## Arrays and HOFs

[= playground =]
def main() =
  val xs = [1, 2, 3, 4, 5]
  print(map(xs, x -> x * x))
  print(reduce(xs, 0, (a, x) -> a + x))
  print(filter(xs, x -> x % 2 == 1))
[= /playground =]

## Complex numbers

[= playground =]
def main() =
  val z = 3.0 + 4.0 * i
  print(z)
  print(z.re)
  print(z.im)
  print(abs(z))             // = 5.0
  print(conj(z))            // = 3.0 - 4.0i
[= /playground =]

## A tiny FFT (recursive, N = 4)

[= playground =]
def fft(x: [complex]): [complex] =
  val n = length(x)
  if n == 1 then return x
  val half = n div 2
  var even = fill(half, 0.0 + 0i)
  var odd  = fill(half, 0.0 + 0i)
  for k in 0..half do
    even[k] = x[2 * k]
    odd[k]  = x[2 * k + 1]
  val ef = fft(even)
  val of = fft(odd)
  var y = fill(n, 0.0 + 0i)
  for k in 0..half do
    val w = cos(-2.0 * pi * to_real(k) / to_real(n)) +
            sin(-2.0 * pi * to_real(k) / to_real(n)) * i
    val t = w * of[k]
    y[k]        = ef[k] + t
    y[k + half] = ef[k] - t
  y

def main() =
  val x: [complex] = [1.0, 1.0, 0.0, 0.0]
  val y = fft(x)
  for k in 0..length(y) do
    print(y[k])
[= /playground =]

## Limitations

- **Single-file programs only.** The playground bypasses the module loader; `import` won't work here. Use multiple `def`s in the same buffer.
- **No file I/O, no stdin.** Output is whatever `print(...)` produces.
- **No `nex compile` path.** This runs the interpreter; the AOT compile path needs `clang` and is JVM-only.
- **Real precision.** Numbers print with Java's `Double.toString` shortest-round-trip here (matching the JVM interpreter exactly). The AOT binary uses `%g` 6-sig-fig — see the [Verification chapter](/tooling/02-verification/) for the documented divergence.

A polished editor (CodeMirror + Nex syntax highlighting) is on the roadmap; the current textarea is intentionally minimal to validate the experience first.
