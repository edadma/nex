---
title: Writing a PPM image with `[byte]`
summary: Build a small gradient image as a `[byte]` buffer in P6 (binary PPM) format and dump it to disk with `write_bytes`.
weight: 140
---

The `[byte]` type is Nex's storage form for binary file I/O. To show it end to end, we build a tiny 64 × 32 image of a smooth red→blue gradient, pack it into the *P6* binary PPM container, and write the result to disk. PPM is convenient here because the on-disk layout is essentially one ASCII header followed by raw RGB triples — exactly the shape `[byte]` is for.

A P6 file is

```
P6
<width> <height>
255
<width * height * 3 bytes of RGB pixel data>
```

with a single newline after each line of the header and **no** terminator between header and pixel block.

For a fixed 64 × 32 image the header is exactly 13 bytes: `P6\n64 32\n255\n`. We build it as a literal `[byte]` and concatenate the pixel block onto it.

```nex
def main() =
  val w = 64
  val h = 32

  // P6\n 6 4   3 2 \n 2 5 5 \n  — 13 ASCII bytes.
  val header_ints = [80, 54, 10, 54, 52, 32, 51, 50, 10, 50, 53, 53, 10]
  val header_len = length(header_ints)
  val pixel_count = w * h
  val buf = bytes(header_len + pixel_count * 3)

  for i in 0..header_len do
    buf[i] = header_ints[i]
  end for

  // Horizontal red→blue gradient with a fixed green channel. The
  // per-pixel index in the buffer is header_len + (y * w + x) * 3.
  for y in 0..h do
    for x in 0..w do
      val base = header_len + (y * w + x) * 3
      val t = to_real(x) / to_real(w - 1)
      buf[base + 0] = to_integer(255.0 * (1.0 - t))   // red
      buf[base + 1] = 32                              // green
      buf[base + 2] = to_integer(255.0 * t)           // blue
    end for
  end for

  write_bytes("gradient.ppm", buf)
  print(s"wrote ${length(buf)} bytes")
```

The program prints `wrote 6157 bytes` (header + 64 × 32 × 3 pixels) and leaves a real `gradient.ppm` on disk that any PPM viewer can open. The same source runs through `nex run` on the interpreter on JVM, Node, and native, and through `nex compile` on the LLVM AOT backend — `write_bytes` routes through the cross-platform runtime so the file appears in the same place on every backend, and the `[byte]` descriptor + ARC helpers in the AOT runtime produce a byte-identical PPM file to the interpreter's.

A real image-processing pipeline would normally widen the byte buffer to `[integer]` (or to `[real]` for HDR / sRGB / linear-light math) with `to_integers`, do the computation, and narrow back to `[byte]` with `to_bytes` before writing. `[byte]` exists for the storage layer; it deliberately does not participate in element-wise arithmetic or fusion. See the Specification, §3.3.1.
