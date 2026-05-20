---
title: Multi-File Modules
summary: Descriptive statistics across three files in one module folder, with inline tests that see private helpers.
weight: 50
---

Folder layout:

```nex
src/
  stats/
    moments.nex          ← module stats
    distribution.nex
    summary.nex
  main.nex
```

**`src/stats/moments.nex`**:

```nex
module stats

def mean(xs: [real]) = sum(xs) / to_real(length(xs))

def variance(xs: [real]) = central_moment(xs, 2)

def stddev(xs: [real]) = sqrt(variance(xs))

// Private to the `stats` module; other files in stats/ can call it
// but importers cannot. The explicit `: real` return type lets sibling
// callers like `variance` see a concrete element type even when the
// body's last expression composes generic prelude HOFs.
private def central_moment(xs: [real], k: integer): real =
  val m = mean(xs)
  val n = to_real(length(xs))
  xs.map(x -> (x - m)^k).sum() / n

// Inline tests sit next to the code they exercise.
// They see private members because they're in the same module.

@test
def test_mean_of_constants() =
  assert_approx(mean([5.0, 5.0, 5.0]), 5.0, 1e-12)

@test
def test_stddev_of_constants_is_zero() =
  assert_approx(stddev(fill(100, 7.0)), 0.0, 1e-12)

@test
def test_central_moment_visible_to_test_in_module() =
  // central_moment is private — only visible because we're in `stats`
  assert_approx(central_moment([1.0, 2.0, 3.0], 1), 0.0, 1e-12)
```

**`src/stats/distribution.nex`**:

```nex
module stats

def skewness(xs: [real]) =
  central_moment(xs, 3) / stddev(xs)^3

def kurtosis(xs: [real]) =
  central_moment(xs, 4) / variance(xs)^2 - 3.0
```

**`src/stats/summary.nex`**:

```nex
module stats

struct Summary
  count: integer
  mean_val: real
  stddev_val: real
  min_val: real
  max_val: real
end

def summarize(xs: [real]) =
  Summary(length(xs), mean(xs), stddev(xs), min(xs), max(xs))
```

**`src/main.nex`**:

```nex
import stats.{Summary, summarize, skewness}

def main() =
  val data = linspace(0.0, 10.0, 100)
  val s = summarize(data)
  print(s"n=${s.count}  mean=${s.mean_val}  sd=${s.stddev_val}")
  print(s"range=[${s.min_val}, ${s.max_val}]")
  print(s"skewness=${skewness(data)}")
```
