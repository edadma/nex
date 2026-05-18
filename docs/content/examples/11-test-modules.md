---
title: Dedicated @test Modules
summary: A sibling test-only module with support code and table-driven fixtures, stripped from non-test builds.
weight: 110
---

When tests need a lot of helpers, fixtures, or table-driven data, put them in a sibling `@test` module. The whole module is stripped from non-test builds.

```nex
@test module stats.distribution_tests

import stats.{mean, variance, stddev, skewness}

// --- support code (not tests, but available to tests in this module) ---

def constant_dataset(n: integer, c: real) = fill(n, c)

def linear_dataset(n: integer) = linspace(0.0, to_real(n - 1), n)

val constant_cases = [
  (constant_dataset(10,    0.0),   0.0),
  (constant_dataset(100,   3.14),  3.14),
  (constant_dataset(1000, -7.5),  -7.5)
]

// --- tests ---

@test
def test_mean_recovers_constants() =
  for sample, c in constant_cases do
    assert_approx(mean(sample), c, 1e-12)
  end for

@test
def test_stddev_of_any_constant_is_zero() =
  for sample, _ in constant_cases do
    assert_approx(stddev(sample), 0.0, 1e-12)
  end for

@test
def test_skewness_of_symmetric_linear_dataset_is_zero() =
  // 0..n-1 is symmetric around (n-1)/2
  assert_approx(skewness(linear_dataset(11)), 0.0, 1e-12)
```

This style is appropriate when:
- The tests need lots of shared setup
- You want fixture data computed once and shared across tests
- The support code is itself testing-specific and would be noise in the regular module

The cost of a `@test` module: it can only see *public* members of the modules it imports. If you need to test `private` helpers, use inline `@test` functions in the same module instead (as in the Multi-File Modules example).
