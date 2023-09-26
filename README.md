fortran-primes
===

This is a tiny Modern Fortran library for handling prime number. 
The base code was spun off the FRESCO CFD code, while additional functions are modern Fortran ports of codes from [Michal Forisek](https://people.ksp.sk/~misof/primes/), [David Deley](https://daviddeley.com/programming/code/primes.htm) and [Primes.jl]([https://github.com/](https://github.com/JuliaMath/Primes.jl).

### API

function      | type | Description 
---        | --- | ---         
`prime(n)` | `integer(IP)`, `integer(WP)` | Returns the $n$-th prime number. 
`primes(n)` | `integer(IP)` | Returns a list containing all primes in the range $1 \lt p_i \le n$.
`primes(lo,hi)` | `integer(IP)` | Returns a list containing all primes in the range $L \le p_i \le H$.
`is_prime(n)` | `logical(LP)` | Boolean flag to check if $n$ is a prime number.
`next_prime(n)` | `integer(IP)` | Returns the first prime number `p_i > n`
`next_prime(n,i)` | `integer(IP)` | Returns the $i$-th ($i \ge 1$) prime number larger than $n$.
`prime_factors(n,f)` | `integer(IP)` | Returns all prime factors, and their multiplicities, of an integer.: 


### 2D Spline interpolators:



Building, using
===============

An automated build is available via the [Fortran Package Manager](https://github.com/fortran-lang/fpm). To use *fortran-primes* within your FPM project, add the following to your fpm.toml file:

```
[dependencies]
fortran-primes = { git="https://github.com/perazz/fortran-primes.git" }
```
