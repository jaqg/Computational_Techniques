# README Final Project #
**José Antonio Quiñonero Gris**

26 November, 2022

## Input data ##
You can change the input data in the file `data/input.dat`.

**Warning**: do not change the order of the lines or add any, just change the
values and/or string if desired.

If the program is already compiled, you can repeat the calculation after
modifying the `input.dat` file just with `./main` (no need to recompile).

## Compilation ##

- You can compile the code with
```bash
$ make
```
- To make a plot of the method used (you need python3)
```bash
$ make plot
```
to also create a plot for comparison of the method *vs* 4-th order Runge Kutta.
```bash
$ make allplots
```
- To remove secondary files
```bash
$ make clean
```
and to remove plots
```bash
$ make cleanplots
```

## Output data ##
You can find the results in the file `data/output.dat`, with a summary of the
input parameters used in the calculation and the results in a tabular form.

Also, there is one unformatted file with the solutions $(t_i\ x_i\ y_i)$ for
each model and method, following the criteria
`data/unform-TheModel(Simple/Logistic)-TheMethod(Taylor,Euler,...).dat`.

## Plots ##
You can also find the style file for the plots as `graph/mine.mplstyle` and
the python scripts to create the plots as `graph/graph.py`, for the method's
plot, and `graph/graph-vs-RK4.py` for the comparison with 4-th order
Runge-Kutta.

The plots theirself are stored in the directory `graph/plots` in `.pdf` format,
with filename following the criteria `Model-Method[-vs-RK4].pdf`.

## Structure of the code ##
My main objective was to make the subprograms as general as posible, in the
sense of having the possibility of applying them in any case.

In that sense, the Lotka-Volterra equations are computed as a function of time
and poblations $y$, as

$$
    \text{LV} \to f = f(t,y)
$$
The methods are computed for any function $f$ as far as the function has the
same form as the computed Lotka-Volterra, meaning any function as $f(t,y)$
$$
    \text{Method} \to \text{call method}(f)
$$
with some other arguments needed for each method.

Either way, the methods can be easily modified for other defined functions just
changing the interface block of the function and its use in the method
subroutine.

### Functions and Subroutines ###
The functions and subroutines are stored in the `subroutines/` directory and,
once compiled, they are included in the `libmylib.a` static library.

I summarize the use of the subprograms in the following.

(**Note**: throughout the following explanations, I usually use array indexing
starting at 0,0 for formulation/code convinience)

#### Lotka-Volterra equations function ####
The function is in the file `subroutines/LV.f90`.

The equations can be written as
$$
    \dfrac{\mathrm{d}y}{\mathrm{d}t} = y' = ay + by^2 + cxy
$$
where $x$ and $y$ are stored in an array $\mathbb{Y}$ in pairs, as
$$
    \mathbb{Y} =
      \begin{pmatrix}
      x(t_0) & y(t_0) & \ldots & z(t_0)  \\
      x(t_1) & y(t_1) & \ldots & z(t_1)  \\
      \vdots & \vdots & \ddots & \vdots  \\
      x(t_f) & y(t_f) & \ldots & z(t_f)  \\
    \end{pmatrix}
$$
so they can be computed as
$$
    \mathbb{Y}_{i+1,j} =
    \mathbb{P}_{1,j} \mathbb{Y}_{i,j} +
    \mathbb{P}_{2,j} \mathbb{Y}_{i,j}^2 +
    \mathbb{P}_{3,j} \mathbb{Y}_{i,j} \mathbb{Y}_{i,j+1}
$$
as there is not an implicit time-dependence. The matrix $\mathbb{P}$ is an
array with the parameters $\alpha, \beta, \ldots$ (explained below)

The LV function is coded as `LV(t, yt)`, so it takes as arguments
- The time `t` (real, double precision, optional)
- The poblation at this time `yt` (real, double precision vector). This
    -already defined and allocated- vector has dimensions of the number of
    species (prey/predator) of the calculation. In our case, 2.
- The parameters $a,b,c$ are passed through a general-use module `modules/io.f90`, in the array `params(:,:)`, represented before as $\mathbb{P}$.
- The output of the function is the updated poblations

#### Taylor's method ####
The subroutine is in the file `modules/taylor_module.f90`.

Taylor's series expansion of a function $y(t)$ around $t=t_0$, considering $y(t=0) = y_0$, is given by
$$
    y(t) = y_0 + y_0' h + \frac{1}{2} y_0'' h^2 + \frac{1}{6} y_0''' h^3 + \ldots
$$
with
$$
    h = t - t_0
$$

So, I computed this as
$$
    y(t_{k+1}) = y_{k+1} = y_k + y_k' h + \frac{1}{2!} y_k'' h^2 + \frac{1}{3!} y_k''' h^3 + \ldots
$$
with
$$
    h = t_{k+1} - t_k
$$
The problem is in calculating the derivatives. I compute the successive
derivatives of $y(t)$ as (for more information read `notes-derivatives-LV.pdf`)
$$
    y^{(n+1)} =
    a y^{(n)} +
    b \sum_{k=0}^{n} \mathbb{C}_{n,k}\, y^{(n-k)} y^{(k)} +
    c \sum_{k=0}^{n} \mathbb{C}_{n,k}\, x^{(n-k)} y^{(k)}
$$
where $\mathbb{C}$ is a coefficient matrix.

Then, I compute the derivatives with the function `derivatives(n, y, ders)`
(contained in the `modules/taylor_module.f90` module), which stores the
evaluated derivatives in an array $\mathbb{D}$ as
$$
    \mathbb{D} =
      \begin{pmatrix}
      x^{(0)} & y^{(0)} & \ldots & z^{(0)} \\
      x^{(1)} & y^{(1)} & \ldots & z^{(1)} \\
       \vdots & \vdots  & \ddots & \vdots  \\
      x^{(N)} & y^{(N)} & \ldots & z^{(N)} \\
    \end{pmatrix}
$$
where $y^{(N)}$ is the $N$-th derivative of $y(t)$ at a time $t$, as $y^{(1)} = \mathrm{d}y^{(0)}(t)/\mathrm{d}t = \mathrm{d}y(t)/\mathrm{d}t$.

The derivatives array $\mathbb{D}$ has dimensions of $N \times n$, for the $N$
first derivatives and $n$ species.

Then, I compute the Taylor's series in the subroutine `Taylor(y0, t0, tf, h, nterms, t, y)` (contained in the `modules/taylor_module.f90` module) as
$$
    \mathbb{Y}_{i+1,j} =
    \sum_{k=0}^{N-1} \frac{1}{k!} \mathbb{D}_{k,j} h^k \qquad N = \text{\# terms}
$$

The `derivatives(n, yt, ders)` subroutine takes as arguments
- The number of derivatives to compute `n` (integer)
- The poblation at this time $\mathbb{Y}(t,:)$, `yt` (real, double precision vector)
- The output of the subroutine is the derivatives array $\mathbb{D}$, `ders` (real, double
    precision 2D array)

The `Taylor(y0, t0, tf, h, nterms, t, y)` subroutine takes as arguments
- The poblations at $t=t_0$, `y0` of each specie (real, double precision vector)
- The initial time $t_0$, `t0` (real, double precision)
- The final time $t_f$, `tf` (real, double precision)
- The time step $h = t_{k+1} - t_{k}$, `h` (real, double precision)
- The number of terms of the expansion, `nterms` (integer)

And outputs

- The time vector `t` (real, double precision vector)
- The poblations matrix $\mathbb{Y}$, `y` (real, double precision array)

So, in reality, you can apply this method to any function by changing the
computation of the array $\mathbb{D}$ acording to the desired function.

#### Euler's method ####
The subroutine is in the file `subroutines/Euler.f90`.

It is the truncation of the Taylor's expansion at first order, and can be
computed as
$$
    y_{n+1} \approx y_n + h f(t_n, y_n)
$$
meaning, in my notation
$$
    \mathbb{Y}_{n+1, j} \approx \mathbb{Y}_{n, j} + h * \mathrm{LV}(t_n, \mathbb{Y}_{n, j})
    = \mathbb{Y}_{n, j} + h \mathbb{D}_{1, j}
$$
Instead of using the `taylor` subroutine with 1 term, I coded it in a separate
file.

The subroutine `Euler(f, y0, t0, tf, h, t, y)` takes as arguments:
- The function -name of the external function subprogram- `f` to be evaluated in the method. In this case, it has to be of
    the form $f = f(t,y)$, but it can be easily changed in the `abstract
    interface` block of the subroutine.
- The poblations at $t=t_0$, `y0` of each specie (real, double precision vector)
- The initial time $t_0$, `t0` (real, double precision)
- The final time $t_f$, `tf` (real, double precision)
- The time step $h = t_{k+1} - t_{k}$, `h` (real, double precision)

And outputs

- The time vector `t` (real, double precision vector)
- The poblations matrix $\mathbb{Y}$, `y` (real, double precision array)

#### Modified Euler's method ####
The subroutine is in the file `subroutines/Modified_Euler.f90`.

In this method, the approximation is achieved with the trapezoid method, and
can be computed as
$$
    y_{k+1} = y_{k+1}^{(n+1)} =
    y_k + \frac{h}{2} \left[ f(t_k, y_k) +
    f \left( t_{k+1}, y_{k+1}^{(n)} \right) \right]
$$
iterating over $n$ until convergence of $y_{k+1}$.

In my notation
$$
    \mathbb{Y}_{k+1, j}^{(n+1)} \approx
    \mathbb{Y}_{k+1, j} + \frac{h}{2} \left[ \mathrm{LV}\left( t_k, \mathbb{Y}_{k, j} \right) +
    \mathrm{LV} \left( t_{k+1}, \mathbb{Y}_{k+1, j}^{(n)} \right) \right]
$$

The subroutine `modEuler(f, y0, t0, tf, h, threshold, t, y)` takes as arguments:
- The function -name of the external function subprogram- `f` to be evaluated in the method. In this case, it has to be of
    the form $f = f(t,y)$, but it can be easily changed in the `abstract
    interface` block of the subroutine.
- The poblations at $t=t_0$, `y0` of each specie (real, double precision vector)
- The initial time $t_0$, `t0` (real, double precision)
- The final time $t_f$, `tf` (real, double precision)
- The time step $h = t_{k+1} - t_{k}$, `h` (real, double precision)
- The threshold for the convergence `threshold` (real, double precision)

And outputs

- The time vector `t` (real, double precision vector)
- The poblations matrix $\mathbb{Y}$, `y` (real, double precision array)

#### Runge-Kutta's method ####
The subroutine is in the file `subroutines/RungeKutta4.f90`.

The fourth-order Runge-Kutta's method is a fourth-order Taylor approximation
and can be written as
$$
    y_{k+1} =
    y_k + \frac{1}{6} \left[ K_1 + 2K_2 + 2K_3 + K_4 \right]
$$
with
$$
    K_1 = h f \left( t_k, y_k \right)
$$
$$
    K_2 = h f \left( t_k + \frac{h}{2}, y_k + \frac{K_1}{2} \right)
$$
$$
    K_3 = h f \left( t_k + \frac{h}{2}, y_k + \frac{K_2}{2} \right)
$$
$$
    K_4 = h f \left( t_k + h, y_k + K_3 \right)
$$

In my notation
$$
    \mathbb{Y}_{k+1, j} =
    \mathbb{Y}_{k, j} + \frac{1}{6} \left[ \mathbb{K}_{1,j} + 2\mathbb{K}_{2,j} + 2\mathbb{K}_{3,j} + \mathbb{K}_{4,j} \right]
$$
with
$$
    \mathbb{K}_{1,j} = h * \mathrm{LV} \left( t_k, \mathbb{Y}_{k, j} \right)
$$
$$
    \mathbb{K}_{2,j} = h * \mathrm{LV} \left( t_k + \frac{h}{2}, \mathbb{Y}_{k, j} + \frac{\mathbb{K}_{1,j}}{2} \right)
$$
$$
    \mathbb{K}_{3,j} = h * \mathrm{LV} \left( t_k + \frac{h}{2}, \mathbb{Y}_{k, j} + \frac{\mathbb{K}_{2,j}}{2} \right)
$$
$$
    \mathbb{K}_{4,j} = h * \mathrm{LV} \left( t_k + h, \mathbb{Y}_{k, j} + \mathbb{K}_{3,j} \right)
$$

The subroutine `RK4(f, y0, t0, tf, h, t, y)` takes as arguments:
- The function -name of the external function subprogram- `f` to be evaluated in the method. In this case, it has to be of
    the form $f = f(t,y)$, but it can be easily changed in the `abstract
    interface` block of the subroutine.
- The poblations at $t=t_0$, `y0` of each specie (real, double precision vector)
- The initial time $t_0$, `t0` (real, double precision)
- The final time $t_f$, `tf` (real, double precision)
- The time step $h = t_{k+1} - t_{k}$, `h` (real, double precision)

And outputs

- The time vector `t` (real, double precision vector)
- The poblations matrix $\mathbb{Y}$, `y` (real, double precision array)
