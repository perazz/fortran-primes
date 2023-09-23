module fortran_primes
  implicit none
  private

  public :: say_hello
contains
  subroutine say_hello
    print *, "Hello, fortran-primes!"
  end subroutine say_hello
end module fortran_primes
