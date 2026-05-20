---
title: Power Iteration
summary: Dominant eigenvalue and eigenvector of a square matrix via the `@` operator and `dot`.
weight: 100
---

```nex
// Power iteration: finds the dominant eigenvalue/eigenvector of a square matrix.
// Returns (eigenvalue, eigenvector).
def power_iteration(A: [[real]], iters: integer) =
  val n = rows(A)
  var v = fill(n, 1.0)               // initial guess — real-typed so subsequent
                                     // reassignments stay [real]; in v0 `ones(n)`
                                     // would give [integer] and the rebind would
                                     // trip the type check.
  var lambda = 0.0

  for k in 0..iters do
    val Av = A @ v                   // matrix-vector multiply (rank-1 result)
    val norm = sqrt(dot(Av, Av))
    v = Av / norm                    // element-wise scalar division
    lambda = dot(v, A @ v)           // Rayleigh quotient
  end for

  (lambda, v)

def main() =
  // Symmetric matrix with eigenvalues 5 and 1
  val A = [[3.0, 2.0],
           [2.0, 3.0]]

  val lambda, v = power_iteration(A, 50)

  print(s"dominant eigenvalue ~ $lambda")
  print(s"corresponding eigenvector ~ $v")

@test
def test_power_iteration_finds_correct_eigenvalue() =
  val A = [[3.0, 2.0], [2.0, 3.0]]
  val lambda, _ = power_iteration(A, 100)
  assert_approx(lambda, 5.0, 1e-6)
```
