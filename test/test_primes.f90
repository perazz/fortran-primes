!   ***********************************************************************************************
!   **                                                                                           **
!   **                  |\   -  -.   ./                                                          **
!   **                  | \./ \/ | ./ /     __________  ___________ __________                   **
!   **                __|         /  /     / ____/ __ \/ ____/ ___// ____/ __ \                  **
!   **                \    .        /.-/  / /_  / /_/ / __/  \__ \/ /   / / / /                  **
!   **                 \   |\.|\/|    /  / __/ / _, _/ /___ ___/ / /___/ /_/ /                   **
!   **                  \__\     /___/  /_/   /_/ |_/_____//____/\____/\____/                    **
!   **                                                                                           **
!   **                                     FRESCO-SpeedCHEM                                      **
!   **            A code for internal combustion engine flows with chemical kinetics             **
!   **                                                                                           **
!   ***********************************************************************************************
!   **                                                                                           **
!   **    test_primes                                                                            **
!   **    Perform prime-related tests                                                            **
!   **                                                                                           **
!   ***********************************************************************************************
!   **                                                                                           **
!   **    Author     : (C) Federico Perini                                                       **
!   **    Created    : 09/23/2023                                                                **
!   **                                                                                           **
!   ***********************************************************************************************
program test_primes

    use prime_numbers
    use prime_constants
    use iso_fortran_env, only: output_unit

    implicit none

    character(*), parameter :: this_test = '[test_primes]'
    integer :: nfailed,npassed

    ! Local variables
    integer      :: i,j,ierr
    character(*), parameter :: fmt_failed = "(1x,a,' has ',i0,' test passed, ',i0,' not passed.')"
    nfailed = 0
    npassed = 0

    ! Perform tests
    call add_test(test_first_10000())
    !call add_test(test_miller_rabin())


    if (nfailed>0) write(*,fmt_failed)this_test,npassed,nfailed

    if (nfailed>0) then
        stop -1
    else
        stop 0
    endif

    return

    contains

    subroutine add_test(success_flag)
        logical, intent(in) :: success_flag
        if (success_flag) then
           npassed = npassed+1
        else
           nfailed = nfailed+1
        end if
    end subroutine add_test

    ! Test first 1000 primes
    logical function test_first_10000() result(success)

        integer(IP) :: tenthousandth

        tenthousandth = p1_to_10000(chunk)

        success = all(primes(tenthousandth) == p1_to_10000) .and. &
                  all(primes(2,tenthousandth) == pack(p1_to_10000,p1_to_10000>=2))

    end function test_first_10000

    ! Test miller rabin test against the table of first primes
    logical function test_miller_rabin() result(success)

       integer :: i,nfail

       nfail = 0

       whole_table: do i=2,10000

          success = miller_rabin_test(witnesses(i),i)
          if (success.neqv.is_prime(i)) then
              print 1, i, is_prime(i)
              nfail = nfail+1
          endif

       end do whole_table

       success = nfail==0

       1 format('miller-rabin: test on ',i0,', is prime=',l1,' not recognized such by miller-rabin test')

    end function test_miller_rabin

    logical function make_min_factors() result(success)

       use fortran_io

       integer, allocatable :: min_factors_table(:)

       ! Generate list of min factors
       integer :: N_SMALL = 2**18

       ! min_factors = the minimum factor of n for odd n, if 3<n<N_SMALL_FACTORS. Return 1 if prime
       min_factors_table = generate_min_factors(N_SMALL)

       call print_1d_array(output_unit,'min_factor',min_factors_table,'IP')
       success = .true.

    end function make_min_factors


end program test_primes

