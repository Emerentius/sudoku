This is a fork of Emerentius/sudoku adding a dynamic library for use with the benchmark program in t-dillon/tdoku.

Benchmark results on several data sets for this (rust_sudoku), two other JCZSolve-family solvers (the original
JCZSolve and SK_BFORCE2), and Tdoku:

<pre>
|puzzles0_kaggle                  |  puzzles/sec|  usec/puzzle|   %no_guess|  guesses/puzzle|
|---------------------------------|------------:| -----------:| ----------:| --------------:|
|jczsolve                         |   594,767.4 |         1.7 |     100.0% |           0.00 |
|sk_bforce2                       | 1,232,766.2 |         0.8 |     100.0% |           0.00 |
|rust_sudoku                      |   723,763.1 |         1.4 |        N/A |            N/A |
|tdoku_dpll_triad_simd            |   994,998.6 |         1.0 |     100.0% |           0.00 |

|puzzles1_17_clue                 |  puzzles/sec|  usec/puzzle|   %no_guess|  guesses/puzzle|
|---------------------------------|------------:| -----------:| ----------:| --------------:|
|jczsolve                         |   290,759.6 |         3.4 |      69.6% |           1.84 |
|sk_bforce2                       |   377,466.5 |         2.6 |      73.7% |           1.00 |
|rust_sudoku                      |   374,938.7 |         2.7 |        N/A |            N/A |
|tdoku_dpll_triad_simd            |   327,404.5 |         3.1 |      78.6% |           0.62 |

|puzzles2_magictour_top1465       |  puzzles/sec|  usec/puzzle|   %no_guess|  guesses/puzzle|
|---------------------------------|------------:| -----------:| ----------:| --------------:|
|jczsolve                         |    77,921.5 |        12.8 |       2.3% |          20.73 |
|sk_bforce2                       |    86,700.8 |        11.5 |       3.6% |          15.43 |
|rust_sudoku                      |    91,203.0 |        11.0 |        N/A |            N/A |
|tdoku_dpll_triad_simd            |   117,044.4 |         8.5 |       7.9% |           9.06 |

|puzzles3_forum_hardest_1905      |  puzzles/sec|  usec/puzzle|   %no_guess|  guesses/puzzle|
|---------------------------------|------------:| -----------:| ----------:| --------------:|
|jczsolve                         |    16,249.8 |        61.5 |       0.0% |         138.67 |
|sk_bforce2                       |    18,148.6 |        55.1 |       0.0% |         103.35 |
|rust_sudoku                      |    19,182.2 |        52.1 |        N/A |            N/A |
|tdoku_dpll_triad_simd            |    24,317.7 |        41.1 |       0.0% |          55.02 |

|puzzles4_forum_hardest_1905_11+  |  puzzles/sec|  usec/puzzle|   %no_guess|  guesses/puzzle|
|---------------------------------|------------:| -----------:| ----------:| --------------:|
|jczsolve                         |    12,617.6 |        79.3 |       0.0% |         170.82 |
|sk_bforce2                       |    14,317.4 |        69.8 |       0.0% |         122.67 |
|rust_sudoku                      |    14,993.1 |        66.7 |        N/A |            N/A |
|tdoku_dpll_triad_simd            |    20,195.2 |        49.5 |       0.0% |          64.98 |

|puzzles5_forum_hardest_1106      |  puzzles/sec|  usec/puzzle|   %no_guess|  guesses/puzzle|
|---------------------------------|------------:| -----------:| ----------:| --------------:|
|jczsolve                         |     6,604.8 |       151.4 |       0.0% |         366.09 |
|sk_bforce2                       |     7,254.8 |       137.8 |       0.0% |         270.94 |
|rust_sudoku                      |     8,039.6 |       124.4 |        N/A |            N/A |
|tdoku_dpll_triad_simd            |    12,871.2 |        77.7 |       0.0% |         113.18 |

|puzzles6_serg_benchmark          |  puzzles/sec|  usec/puzzle|   %no_guess|  guesses/puzzle|
|---------------------------------|------------:| -----------:| ----------:| --------------:|
|jczsolve                         |   309,916.7 |         3.2 |       0.0% |           7.09 |
|sk_bforce2                       |   347,875.3 |         2.9 |       0.0% |           7.08 |
|rust_sudoku                      |   392,788.2 |         2.5 |        N/A |            N/A |
|tdoku_dpll_triad_simd            |   395,719.9 |         2.5 |       0.0% |           7.13 |

|puzzles7_gen_puzzles             |  puzzles/sec|  usec/puzzle|   %no_guess|  guesses/puzzle|
|---------------------------------|------------:| -----------:| ----------:| --------------:|
|jczsolve                         | 1,765,997.6 |         0.6 |      97.5% |           0.32 |
|sk_bforce2                       | 1,996,367.2 |         0.5 |      97.6% |           0.31 |
|rust_sudoku                      | 1,081,562.2 |         0.9 |        N/A |            N/A |
|tdoku_dpll_triad_simd            | 3,011,905.8 |         0.3 |      97.5% |           0.28 |
</pre>

Run on i5-8600k clang-8 -O3 -march=native
