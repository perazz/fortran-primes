fortran-primes
===

This is a tiny Modern Fortran library for handling prime number. 
The base code was spun off the FRESCO CFD code, while additional functions are modern Fortran ports of codes from [](), []() and [Primes.jl]([https://github.com/](https://github.com/JuliaMath/Primes.jl).
The functions are modernized and translated from the original Fortran77 code [FITPACK](http://www.netlib.org/dierckx) by Paul Dierckx.
The starting code used the double precision version of FITPACK distributed with [scipy](http://www.scipy.org).

An object-oriented interface wrapper is also being built:

### 1D Spline interpolators:

Class      | Description | Degree
---        | ---         | ---
`fitpack_curve` | 1D spline interpolation of scattered data, $y = s(x)$ | up to 5
`fitpack_parametric_curve` | Parametric 1D curves in N dimensions, $x_i = s_i(u)$, $i=1,\ldots,n$ | up to 5
`fitpack_closed_curve` | Closed parametric 1D curves in N dimensions, $x_i = s_i(u)$, $i=1,\ldots,n$, $x_i(0)=x_i(1)$ | up to 5
`fitpack_constrained_curve` | Parametric 1D curves in N dimensions with value/derivative constraints at the endpoints $x_i = s_i(u)$, $i=1,\ldots,n$, $x_{i}^{(j)}(0)=u_{L,i}^{(j)}$ , $x_{i}^{(j)}(1)=u_{R,i}^{(j)}$, $0\le j \le 2$| up to 5

### 2D Spline interpolators:

Class      | Description | Degree
---        | ---         | ---
`fitpack_surface` | 2D spline interpolation of scattered data, $z = s(x,y)$ | up to 5
`fitpack_polar` | 2D spline interpolation of scattered data in a user-defined polar domain $z = s(u,v)$, $u\in[0,1]$, $v\in[-\pi,\pi]$, user-defined domain radius as a function of polar angle $r=r(v)$ | 3
`fitpack_sphere` | 2D spline interpolation of scattered data on a sphere domain $z = s(u,v)$ with latitude $u \in [0,\pi]$, longitude $v \in [-\pi,\pi]$ | 3
`fitpack_grid_surface` | 2D spline interpolation of rectangular 2D data $z = s(x,y)$ with gridded fitting coordinates $x_i, i=1,\ldots,n_x$,  $y_j, j=1,\ldots,n_y$  | up to 5
`fitpack_grid_polar` | 2D spline interpolation of polar data $z = s(u,v)$ in the fixed-radius circular polar domain $u\in[0,r]$, $v\in[-\pi,\pi]$, with user-control of function and derivatives at the origin and the boundaries | 3

Building, using
===============

An automated build is available via the [Fortran Package Manager](https://github.com/fortran-lang/fpm). To use *fortran-primes* within your FPM project, add the following to your fpm.toml file:

```
[dependencies]
fitpack = { git="https://github.com/perazz/fortran-primes.git" }
```
