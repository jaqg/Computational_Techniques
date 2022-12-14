# Computational Techniques

## [Cholesky decomposition](https://github.com/jaqg/Computational_Techniques/tree/main/exercises/cholesky_decomposition) ##


### Example Output ###

```
+------------------+
| Program Cholesky |
+__________________+

 The dimension of the matrix is
 n =           5

------------------------------
 Random-generated vector
   5.0
   1.0
   0.0
   7.0
   1.0
------------------------------
 Original matrix, M0
  25.0   5.0   0.0  35.0   5.0
   5.0   1.0   0.0   7.0   1.0
   0.0   0.0   0.0   0.0   0.0
  35.0   7.0   0.0  49.0   7.0
   5.0   1.0   0.0   7.0   1.0
------------------------------


 --- ITERATIONS ---

==============================
 J =           1
------------------------------
 Matrix M
  25.0   5.0   0.0  35.0   5.0
   5.0   1.0   0.0   7.0   1.0
   0.0   0.0   0.0   0.0   0.0
  35.0   7.0   0.0  49.0   7.0
   5.0   1.0   0.0   7.0   1.0

 Vector L
  5.0
  1.0
  0.0
  7.0
  1.0

 New matrix M
   0.0   0.0   0.0   0.0   0.0
   0.0   0.0   0.0   0.0   0.0
   0.0   0.0   0.0   0.0   0.0
   0.0   0.0   0.0   0.0   0.0
   0.0   0.0   0.0   0.0   0.0
==============================


 --- FINAL RESULTS ---

 Total number of iterations:           1

------------------------------
 Matrix L
   5.0   0.0   0.0   0.0   0.0
   1.0   0.0   0.0   0.0   0.0
   0.0   0.0   0.0   0.0   0.0
   7.0   0.0   0.0   0.0   0.0
   1.0   0.0   0.0   0.0   0.0
------------------------------
```
